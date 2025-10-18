-module(kiss_cache_fetcher).

% Copyright (c) CrankWheel ehf. 2025
% Author: Jói Sigurdsson https://joisig.com/

-behaviour(gen_server).
-export([start_link/1, init/1, handle_cast/2, handle_call/3, handle_info/2]).

% This module does the tiniest amount of work it can for each message
% it receives, to avoid having requests delays pile up.
%
% While its clients wait synchronously,
% the process does most things asynchronously, spawning workers and responding
% to callers by explicitly replying to them when a worker completes.
%
% Thus, if we are unlucky, and say there are 10 different requests, each of
% which would take about 200 ms to process (e.g. on the SS node) and they
% all go to a single fetcher, instead of all the requests being serialized
% and taking a combined roughly 2000 ms, different workers are spawned almost
% simultaneously and may all complete almost simultaneously so the overall
% wait is only a bit above 200 ms.
%
% For each new fetch request, there are three possibilities:
% a) Request has been cached; {reply, ...} immediately with the cache value;
% b) Request is not yet being processed; spawn a new worker process to call kiss_cache:fetch
%    for it and {noreply, ...} immediately (we send an explicit gen_server:reply() when the
%    worker process completes); or
% c) Request is being processed; add this request to the list of those waiting to be replied
%    to as in (b).

start_link(Name) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  true = erlang:register(Name, Pid),
  {ok, Pid}.

% State is a map of {fetch, M:F(A)} tuples (as per kiss_cache:make_fetch_key/5) as keys to a list
% of From items waiting for a result, and {monitor, Pid} tuples as keys, to the cache key
% being retrieved by that worker process.
%
% A {fetch, M:F(A)} key is only in the state map if we are currently waiting for a worker to
% complete making the fetch for that M:F(A), and value is a list of From items with at least one item.
%
% A {monitor, Pid} key is also only in the state map if we are currently waiting
% for a worker to complete making a fetch.
init(_Args) ->
  {ok, #{}}.

% Can be tested with something like:
%
% lists:map(fun(_) -> spawn(fun() -> Result = kiss_cache:fetch(ss_cache, net, module_info, []), io:format("~w\n", [Result]) end) end, lists:seq(1,100)).
%
% By uncommenting the io:format lines below you can verify that it's functioning correctly.
handle_call({fetch, Cache, Module, Func, Params, CheckReturn, ApplyFunc, ExpiryMs, ShouldCleanup}, From, State) ->
  %io:format("Fetcher running!\n"),
  FetchKey = kiss_cache:make_fetch_key(Module, Func, Params, CheckReturn, ApplyFunc),
  case kiss_cache:get(Cache, FetchKey, kiss_cache_uncached) of
    kiss_cache_uncached ->
      case maps:get(FetchKey, State, no_such_worker) of
        no_such_worker ->
          % Not being fetched yet. We start a worker and add our caller as a waiter.
          Parent = self(),
          Pid = spawn(fun () ->
            %io:format("Worker running!\n", []),
            Result = kiss_cache:fetch_noserialize(Cache, Module, Func, Params, CheckReturn, ApplyFunc, ExpiryMs, ShouldCleanup),
            Parent ! {fetch_complete, self(), FetchKey, Result}
          end),
          % We monitor rather than link, so that we can continue to have fault isolation between
          % processes using the cache. Imagine a bunch of processes involved in some meeting A,
          % making the same request to fetch via the cache, and another bunch of processes
          % involved in some meeting B, making a different request to fetch via the cache. We
          % don't want the processes involved with A to get errors even if there is an error
          % fetching for the processes involved with B. We therefore monitor for abnormal
          % exits, and when it happens we respond with a result that lets the abnormal exit
          % reason be thrown as an exception in the process(es) calling the cache.
          monitor(process, Pid),
          % Store the caller as a waiter under FetchKey, and store the FetchKey under
          % {monitor, Pid} so that we can resolve if the worker process exits abnormally.
          StateWithFetchKey = maps:put(FetchKey, [From], State),
          StateWithMonitorKey = maps:put({monitor, Pid}, FetchKey, StateWithFetchKey),
          {noreply, StateWithMonitorKey};
        [_FirstWaiter|_] = Waiters when is_list(Waiters) ->
          % Already being fetched. We add our caller as a waiter.
          {noreply, maps:put(FetchKey, [From|Waiters], State)}
      end;
    Result ->
      {reply, Result, State}
  end.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({fetch_complete, Pid, FetchKey, Result}, State) ->
  NewState = send_result(Pid, FetchKey, Result, State),
  {noreply, NewState};

handle_info({'DOWN', _Ref, process, _Pid, normal}, State) ->
  % We already would have received fetch_complete and removed the monitor from our
  % state, so no need to do anything here.
  {noreply, State};
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
  FetchKey = maps:get({monitor, Pid}, State),
  Result = {kiss_cache_fetcher_abnormal, Reason},
  NewState = send_result(Pid, FetchKey, Result, State),
  {noreply, NewState}.

send_result(Pid, FetchKey, Result, State) ->
  StateWithoutMonitorKey = maps:remove({monitor, Pid}, State),
  % This asserts a non-empty list is taken from the map. We are left with a map
  % with no entry for FetchKey, and no entry for our monitored Pid.
  {[_FirstWaiter|_] = Waiting, NewState} = maps:take(FetchKey, StateWithoutMonitorKey),
  lists:map(fun(W) -> gen_server:reply(W, Result) end, Waiting),
  NewState.
