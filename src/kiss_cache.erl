-module(kiss_cache).

% Copyright (c) CrankWheel ehf. 2025
% Author: Jói Sigurdsson https://joisig.com/

% Close to simplest possible timed-expiry cache you can think of for
% Erlang, based on ETS.
%
% You can put stuff into the cache and specify expiry.
%
% You can have public function calls with given parameters cached.
%
% The basic put and get calls rely on ETS synchronization only, adding
% no transactions or processes, as does the fetch_noserialize function.
%
% When using any of the above, when a value is missing from the cache,
% it's possible there will be multiple competing processes that write a
% new value to that key (for keys that are being fetched very frequently).
% This is OK for use-cases where it's sufficient for the cache to be best-effort
% and for writes to the cache to fail occasionally, or not be seen
% by all users of the cache immediately.
%
% For use-cases where we need to minimize hits to the resource underlying
% the cache (e.g. a database) and there is a possibility for a very large
% number of requestors to appear, for the same data, within a very short
% timespan (virtually simultaneously), we use a set of kiss_cache_fetcher
% processes to serialize all fetches for the same
% M:F(A) if the result is not already in the cache. The effect of this
% is that only the first request (for short bursts of requests, shorter
% than the cache expiration of the item fetched) will do the work needed
% to fetch the data, all others will respond via the cache. This is done
% without any ETS-based transactions, and relying rather on Erlang's
% built in serialization of messages to processes. For scalability, we
% have a number of fetcher processes that are indexed, and the process
% to use is based on a hash of the M:F(A) being fetched. We further avoid
% pile-ups of long call timeouts by utilizing asynchronous handling in
% that process, with a worker for each M:F(A) (see details in
% kiss_cache_fetcher.erl).
%
% If you need to use fetch/{4,5,6,8}, you must link kiss_cache_sup
% into your app's supervision tree.
%
% Cache cleanup is done lazily only:
% - On read, if we read an expired item, we delete just that item. Note
%   that this can race between different processes and result in a
%   newly-written entry being deleted almost immediately. Again, this
%   simplistic behavior is OK for use-cases where it's OK for the cache
%   to be best-effort.
% - On write, by default in 1 out of every 1.000 writes (randomized) we
%   spawn a new cleanup process that deletes all expired items. 1.000 is
%   chosen based on an assumption that entries will be less than 100 Kb
%   each on average, which would mean an (amortized) maximum size of
%   already-expired entries of less than 100 Mb.
% - If you need different behavior for full cleanups, you can set the
%   ShouldCleanUp parameter of put, or if you want to clean up on a
%   fixed schedule, you can explicitly call the
%   delete_all_expired/1 function which is what normally gets spawned
%   lazily as described above.
%
% No supervisor or watchdog process is needed due to the way expiration
% is done. Therefore this module can be reused as plain old code.
%
% The motivation for writing this very simple cache is that Cachex, which
% we were using, showed itself at the time (~2021) to sometimes deadlock
% under heavy load, most likely due to transactions that are set up on
% "retrieve or set" type of operations on the cache. We needed something
% foolproof but knew it could be a lot simpler than Cachex. Note that we
% have not tested Cachex since ~2021 so we are not claiming it has any
% problems today, but kiss_cache could still be a good option for you if
% you want something dead simple that you can easily verify.

-export([start_cache/1]).
-export([put/3, put/4, put/5]).
-export([get/2, get/3]).
-export([delete/2]).
-export([fetch/4, fetch/5, fetch/6, fetch/8, fetch_noserialize/8]).
-export([make_fetch_key/5]).
-export([delete_all_expired/1, delete_all/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([test_echo/1]).
-endif.

-define(EXPIRY_MS, 10000).
-define(CLEANUP_CHANCE, 1000).

% Create a new cache table. Do this once per table before using them.
% Note that this must be done from a long-lived process, as the table
% lifetime is scoped to the lifetime of the process calling this.
%
% If you are using kiss_cache_sup, you can pass it a list of caches
% you would like it to start and own via kiss_cache_sup:start_link/1.
start_cache(Cache) ->
  Cache = ets:new(Cache, [named_table, public, set, {keypos, 1}]),
  Cache.

% Put a key into the cache.
%
% Expiry is default 10000 ms.
%
% Our cleanup function is not spawned if ShouldCleanUp is 0 or false, always spawned if
% set to 1 or true. Otherwise it is spawned with a random chance of 1/ShouldCleanUp.
put(Cache, Key, Val) ->
  put(Cache, Key, Val, ?EXPIRY_MS).

put(Cache, Key, Val, ExpiryMs) ->
  put(Cache, Key, Val, ExpiryMs, ?CLEANUP_CHANCE).

put(Cache, Key, Val, ExpiryMs, true) ->
  put(Cache, Key, Val, ExpiryMs, 1);
put(Cache, Key, Val, ExpiryMs, false) ->
  put(Cache, Key, Val, ExpiryMs, 0);
put(Cache, Key, Val, ExpiryMs, ShouldCleanUp) when ShouldCleanUp >= 0 ->
  true = ets:insert(Cache, {Key, os:system_time(millisecond) + ExpiryMs, Val}),
  DoCleanUp = case ShouldCleanUp of
    0 ->
      false;
    _ ->
      case rand:uniform(ShouldCleanUp) of
        1 -> true;
        _ -> false
      end
  end,
  case DoCleanUp of
    true ->
      cleanup_impl(Cache),
      ok;
    false ->
      ok
  end.

% Get a value from the cache. Optionally, specify Default to return if no
% value exists in the cache or it is not fresh enough, otherwise nil is
% returned by default.
get(Cache, Key) ->
  get(Cache, Key, nil).

get(Cache, Key, Default) ->
  case ets:lookup(Cache, Key) of
    [] ->
      Default;
    [{Key, ExpiresMs, Value}] ->
      NowMs = os:system_time(millisecond),
      case NowMs > ExpiresMs of
        true ->
          delete(Cache, Key),
          Default;
        false ->
          Value
      end
  end.

delete(Cache, Key) ->
  ets:delete(Cache, Key).

% Call module:func.(params), caching the result unless there is an error.
%
% For further control, you can provide a check_return fn which receives a
% single parameter, the result of the uncached function. It should return
% either :commit or :ignore depending on whether the result should be
% stored in the cache (:commit) or passed through and not placed in the
% cache (:ignore).
fetch(Cache, Module, Func, Params) ->
  fetch(Cache, Module, Func, Params, nil, fun apply/3, ?EXPIRY_MS, ?CLEANUP_CHANCE).

fetch(Cache, Module, Func, Params, CheckReturn) when is_function(CheckReturn) ->
  fetch(Cache, Module, Func, Params, CheckReturn, fun apply/3, ?EXPIRY_MS, ?CLEANUP_CHANCE);
fetch(Cache, Module, Func, Params, ExpiryMs) when is_integer(ExpiryMs) ->
  fetch(Cache, Module, Func, Params, nil, fun apply/3, ExpiryMs, ?CLEANUP_CHANCE).

fetch(Cache, Module, Func, Params, ExpiryMs, ShouldCleanup) when is_integer(ExpiryMs) and is_integer(ShouldCleanup) ->
  fetch(Cache, Module, Func, Params, nil, fun apply/3, ExpiryMs, ShouldCleanup).

fetch(Cache, Module, Func, Params, CheckReturn, ApplyFunc, ExpiryMs, ShouldCleanup) ->
  Key = make_fetch_key(Module, Func, Params, CheckReturn, ApplyFunc),
  case get(Cache, Key, kiss_cache_uncached) of
    kiss_cache_uncached ->
      case kiss_cache_sup:fetcher_pid_for_query(Key) of
        undefined ->
          % Not using lager to log as it's not available under ss
          io:format("WARNING kiss_cache: fetcher process not alive for key ~w, falling back to unserialized fetch.", [Key]),
          fetch_noserialize(Cache, Module, Func, Params, CheckReturn, ApplyFunc, ExpiryMs, ShouldCleanup);
        Pid ->
          case gen_server:call(Pid, {fetch, Cache, Module, Func, Params, CheckReturn, ApplyFunc, ExpiryMs, ShouldCleanup}, infinity) of
            {kiss_cache_fetcher_abnormal, Reason} ->
              throw(Reason);  % Probably won't be handled, but there may be cases where it could be.
            Result ->
              Result
          end
      end;
    Result ->
      Result
  end.

fetch_noserialize(Cache, Module, Func, Params, CheckReturn, ApplyFunc, ExpiryMs, ShouldCleanup) ->
  Key = make_fetch_key(Module, Func, Params, CheckReturn, ApplyFunc),
  case get(Cache, Key, kiss_cache_uncached) of
    kiss_cache_uncached ->
      Result = ApplyFunc(Module, Func, Params),
      case CheckReturn of
        nil ->
          put(Cache, Key, Result, ExpiryMs, ShouldCleanup),
          Result;
        _ ->
          case CheckReturn(Result) of
            commit ->
              put(Cache, Key, Result, ExpiryMs, ShouldCleanup),
              Result;
            ignore ->
              Result
          end
      end;
    Result ->
      Result
  end.

make_fetch_key(Module, Func, Params, CheckReturn, ApplyFunc) ->
  {fetch, Module, Func, Params, CheckReturn, ApplyFunc}.

delete_all_expired(Cache) ->
  ets:select_delete(Cache, get_match_object()).

delete_all(Cache) ->
  ets:delete_all_objects(Cache).


%
% Private
%

get_match_object() ->
  % Note: To figure out a match object, the best way is to use the ets:fun2ms/1 function
  % which is available in the Erlang shell. See https://learnyousomeerlang.com/ets for
  % details.
  [{{'_','$1','_'},[{'<','$1', os:system_time(millisecond) }],[true]}].

% We do cleanup within the put function when testing to make it deterministic, but
% spawned to a separate process in production (to avoid a put operation taking
% much longer than expected for the calling process).
-ifdef(TEST).
cleanup_impl(Cache) ->
  delete_all_expired(Cache).
-else.
cleanup_impl(Cache) ->
  spawn(fun() -> delete_all_expired(Cache) end).
-endif.


%
% Tests
%

-ifdef(TEST).

test_setup() ->
  catch ets:delete(cx),
  catch ets:delete(cxmeta),
  start_cache(cx),
  start_cache(cxmeta).

test_echo(Param) ->
  put(cxmeta, echo, [Param|get_echoes()]),
  Param.

get_echoes() ->
  get(cxmeta, echo, []).

put_get_test() ->
  test_setup(),
  put(cx, hello, world),
  ?assertEqual(world, get(cx, hello)).

no_cleanup_test() ->
  test_setup(),
  put(cx, expired, foo, 0),
  lists:map(fun(_) ->
    put(cx, new, boo, 1, false)
  end, lists:seq(1, 10000)),
  ?assertEqual(2, ets:info(cx, size)).

explicit_cleanup_test() ->
  test_setup(),
  put(cx, expired1, foo, -1),
  put(cx, expired2, boo, -1),
  delete_all_expired(cx),
  ?assertEqual(0, ets:info(cx, size)).

implicit_cleanup_test() ->
  test_setup(),
  put(cx, expired, foo, -1),
  put(cx, new, boo, 500, true),
  ?assertEqual(1, ets:info(cx, size)),
  ?assertEqual(boo, get(cx, new)).

lazy_cleanup_test() ->
  test_setup(),
  put(cx, expired, foo, -1),
  nil = get(cx, expired),
  ?assertEqual(0, ets:info(cx, size)).

fetch_for_testing(Cache, M, F, A, CheckReturn) ->
  fetch_noserialize(Cache, M, F, A, CheckReturn, fun apply/3, ?EXPIRY_MS, ?CLEANUP_CHANCE).

fetch_test() ->
  test_setup(),
  fetch_for_testing(cx, kiss_cache, test_echo, [1], nil),
  ?assertEqual([1], get_echoes()),
  fetch_for_testing(cx, kiss_cache, test_echo, [1], nil),  % Same param, should be  cached.
  ?assertEqual([1], get_echoes()),
  fetch_for_testing(cx, kiss_cache, test_echo, [2], nil),  % Different.
  ?assertEqual([2, 1], get_echoes()).

fetch_with_inspect_test() ->
  test_setup(),
  CommitReturn = fun(_) -> commit end,
  IgnoreReturn = fun(_) -> ignore end,
  fetch_for_testing(cx, kiss_cache, test_echo, [1], CommitReturn),
  ?assertEqual([1], get_echoes()),
  ?assertEqual(1, ets:info(cx, size)),
  fetch_for_testing(cx, kiss_cache, test_echo, [1], CommitReturn),  % Delivered from cache.
  ?assertEqual([1], get_echoes()),
  ?assertEqual(1, ets:info(cx, size)),
  UncachedResult = fetch_for_testing (cx, kiss_cache, test_echo, [2], IgnoreReturn),
  ?assertEqual(2, UncachedResult),
  ?assertEqual([2, 1], get_echoes()),
  ?assertEqual(1, ets:info(cx, size)),
  ?assertEqual(nil, get(cx, {fetch, kiss_cache, test_echo, [2], IgnoreReturn, fun apply/3})).

fetch_serialize_test() ->
  test_setup(),
  {ok, Pid} = supervisor:start_link(kiss_cache_sup, []),
  link(Pid),
  fetch(cx, kiss_cache, test_echo, [upstream_only_called_once]),
  ?assertEqual([upstream_only_called_once], get_echoes()),
  fetch(cx, kiss_cache, test_echo, [upstream_only_called_once]),
  ?assertEqual([upstream_only_called_once], get_echoes()),
  fetch(cx, kiss_cache, test_echo, [different_value]),
  ?assertEqual([different_value, upstream_only_called_once], get_echoes()),
  exit(Pid, normal).

fetch_serialize_stress_test() ->
  test_setup(),
  {ok, Pid} = supervisor:start_link(kiss_cache_sup, []),
  link(Pid),
  lists:map(fun(_) -> spawn(fun() -> Result = fetch(cx, kiss_cache, test_echo, [upstream_only_called_once], 15000) end) end, lists:seq(1, 1000)),
  timer:sleep(100),
  ?assertEqual([upstream_only_called_once], get_echoes()),
  exit(Pid, normal).

fetch_serialize_error_test() ->
  test_setup(),
  {ok, Pid} = supervisor:start_link(kiss_cache_sup, []),
  link(Pid),
  ?assertThrow(_, fetch(cx, kiss_cache, no_such_function, [])),
  exit(Pid, normal).

-endif.
