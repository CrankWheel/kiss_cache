-module(kiss_cache_sup).

% Copyright (c) CrankWheel ehf. 2025
% Author: Jói Sigurdsson https://joisig.com/

-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1]).
-export([fetcher_pid_for_query/1]).

-define(NUM_FETCHERS, 12).

start_link() ->
  start_link([]).

start_link(Caches) ->
  lists:map(fun(Cache) ->
    Cache = kiss_cache:start_cache(Cache)
  end, Caches),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 10, 10},
    lists:map(fun (Id) ->
      Name = make_name(Id),
      {Name, {kiss_cache_fetcher, start_link, [Name]},
       permanent, brutal_kill, worker, [kiss_cache_fetcher]}
    end, lists:seq(0, ?NUM_FETCHERS - 1))}}.

fetcher_pid_for_query(HashableTerm) ->
  Name = make_name(erlang:phash2(HashableTerm, ?NUM_FETCHERS)),
  whereis(Name).

make_name(Index) ->
  list_to_atom(lists:flatten(io_lib:format("kiss_cache_fetcher_~w", [Index]))).
