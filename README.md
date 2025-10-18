# kiss_cache

A simple, fast ETS-based cache with timed expiry for Erlang and Elixir.

Authored by Jói Sigurdsson (joi@crankwheel.com, https://joisig.com/) and copyright
CrankWheel ehf. Initially developed for CrankWheel Screen Sharing back in 2021.

## Features

- **Simple API**: Basic `put/get/delete` operations with minimal overhead
- **Timed expiry**: Automatic expiration of cached entries
- **Lazy cleanup**: Efficient memory management without background processes
- **Serialized fetching**: Optional fetch serialization to prevent thundering herd problems
- **Zero dependencies**: Uses only Erlang/OTP stdlib
- **Production tested**: Powers high-traffic applications

## Philosophy

`kiss_cache` (Keep It Simple, Stupid) is designed to be the simplest possible timed-expiry cache you can think of for Erlang/Elixir. It's based on ETS and adds minimal complexity while solving common caching problems.

The motivation for writing this cache was that other solutions we tried would sometimes deadlock under heavy load, likely due to transactions. We needed something foolproof but simpler.

This implementation is fairly battle tested with several years in the field and some high-contention use cases that have happened multiple times.

## Installation

### Elixir (mix)

Add to your `mix.exs`:

```elixir
def deps do
  [
    {:kiss_cache, "~> 1.0"}
  ]
end
```

### Erlang (rebar3)

Add to your `rebar.config`:

```erlang
{deps, [
  {kiss_cache, "1.0.0"}
]}.
```

## Usage

> **⚠️ Important: ETS Table Ownership**
>
> Cache tables are ETS tables owned by the process that creates them. If that process dies, the cache is deleted. **Always create caches from a long-lived, supervised process.**
>
> **Recommended approach:** Pass cache names to the supervisor:
> ```erlang
> {ok, _Pid} = kiss_cache_sup:start_link([my_cache, other_cache]).
> ```
>
> **Alternative:** Call `start_cache/1` from your own supervised GenServer/worker.
>
> See [Architecture](#architecture) for integration details.

### Basic Usage

```erlang
% Create a cache table (do this once, from a long-lived process)
kiss_cache:start_cache(my_cache).

% Put values with default 10 second expiry
kiss_cache:put(my_cache, my_key, my_value).

% Put with custom expiry (in milliseconds)
kiss_cache:put(my_cache, my_key, my_value, 60000).  % 60 seconds

% Get values
Value = kiss_cache:get(my_cache, my_key).

% Get with default if not found
Value = kiss_cache:get(my_cache, my_key, default_value).

% Delete a key
kiss_cache:delete(my_cache, my_key).
```

### Elixir Example

```elixir
# Create cache
:kiss_cache.start_cache(:my_cache)

# Store and retrieve
:kiss_cache.put(:my_cache, :user_123, %{name: "Alice"})
user = :kiss_cache.get(:my_cache, :user_123)

# With custom expiry (30 seconds)
:kiss_cache.put(:my_cache, :session, session_data, 30_000)
```

### Serialized Fetch (Advanced)

For expensive operations that many processes might request simultaneously, use `fetch/4+` to serialize the computation and cache the result:

```erlang
% First, start the supervisor with your cache names (do this once in your app)
% This creates the caches AND supervises the fetcher processes
{ok, Pid} = kiss_cache_sup:start_link([my_cache]).

% Now fetch will serialize calls to expensive_function/1
Result = kiss_cache:fetch(my_cache, my_module, expensive_function, [arg1]).

% Even if 1000 processes call this simultaneously, expensive_function
% will only be called once (assuming the result isn't already cached).
```

The serialized fetch prevents "thundering herd" problems where many processes simultaneously try to compute the same expensive value.

**Note:** Passing cache names to `start_link/1` ensures the supervisor owns the ETS tables, guaranteeing they survive for the lifetime of your application.

### Cleanup Behavior

Cache cleanup is lazy and efficient:

- **On read**: Expired items are deleted when accessed
- **On write**: By default, 1 in every 1000 writes triggers a full cleanup (spawned in background)
- **Manual**: Call `kiss_cache:delete_all_expired/1` to clean up explicitly

You can control cleanup behavior:

```erlang
% Never trigger cleanup on this write
kiss_cache:put(my_cache, key, value, 10000, false).

% Always trigger cleanup on this write
kiss_cache:put(my_cache, key, value, 10000, true).

% Trigger cleanup 1 in N times (here: 1 in 500)
kiss_cache:put(my_cache, key, value, 10000, 500).
```

## API Reference

### Core Functions

- `start_cache(Cache)` - Create a new cache table
- `put(Cache, Key, Value)` - Store with default 10s expiry
- `put(Cache, Key, Value, ExpiryMs)` - Store with custom expiry
- `put(Cache, Key, Value, ExpiryMs, ShouldCleanup)` - Store with cleanup control
- `get(Cache, Key)` - Retrieve value (returns `nil` if not found/expired)
- `get(Cache, Key, Default)` - Retrieve with custom default
- `delete(Cache, Key)` - Delete a key
- `delete_all_expired(Cache)` - Manually clean up all expired entries
- `delete_all(Cache)` - Clear the entire cache

### Fetch Functions (Requires Supervisor)

- `fetch(Cache, Module, Func, Params)` - Serialized cached function call
- `fetch(Cache, Module, Func, Params, CheckReturn)` - With result validation
- `fetch(Cache, Module, Func, Params, ExpiryMs)` - With custom expiry
- `fetch(Cache, Module, Func, Params, ExpiryMs, ShouldCleanup)` - Full control

## Architecture

- **kiss_cache**: Core module with put/get/delete operations
- **kiss_cache_sup**: Supervisor for fetch processes (optional)
- **kiss_cache_fetcher**: gen_server pools for serializing fetches (optional)

Basic operations (put/get/delete) require no supervisor. Only use the supervisor if you need the `fetch/*` functions for serialized fetching.

### Table Ownership and Supervision

Cache tables are ETS tables with ownership semantics: the table belongs to the process that creates it and is automatically deleted when that process terminates.

**Integration patterns:**

1. **Using kiss_cache_sup (Recommended)**

   **Erlang:**
   ```erlang
   % In your application supervisor
   init([]) ->
     Children = [
       {kiss_cache_sup, {kiss_cache_sup, start_link, [[cache1, cache2]]},
        permanent, 5000, supervisor, [kiss_cache_sup]},
       % ... other children
     ],
     {ok, {{one_for_one, 10, 10}, Children}}.
   ```

   **Elixir:**
   ```elixir
   # In your application supervisor
   def init(_) do
     children = [
       {kiss_cache_sup, [[cache1, cache2]]},
       # ... other children
     ]

     Supervisor.init(children, strategy: :one_for_one)
   end
   ```

   The supervisor creates and owns the caches, ensuring they persist for your app's lifetime.

2. **Custom worker pattern**

   **Erlang:**
   ```erlang
   % In your own GenServer's init/1
   init(_Args) ->
     kiss_cache:start_cache(my_cache),
     {ok, #state{}}.
   ```

   **Elixir:**
   ```elixir
   # In your own GenServer's init/1
   def init(_args) do
     :kiss_cache.start_cache(:my_cache)
     {:ok, %{}}
   end
   ```

   Your GenServer owns the cache. If your GenServer is supervised, the cache survives restarts.

3. **Basic operations only (no supervisor needed)**

   **Erlang:**
   ```erlang
   % Create cache from a long-lived process
   kiss_cache:start_cache(my_cache),

   % Use basic operations
   kiss_cache:put(my_cache, key, value),
   kiss_cache:get(my_cache, key).
   ```

   **Elixir:**
   ```elixir
   # Create cache from a long-lived process
   :kiss_cache.start_cache(:my_cache)

   # Use basic operations
   :kiss_cache.put(:my_cache, :key, :value)
   :kiss_cache.get(:my_cache, :key)
   ```

   Suitable if you don't need the `fetch/*` serialization features.

**Warning:** Never create caches from the Erlang shell or temporary processes in production, as they will be deleted when that process terminates.

## Trade-offs

This cache is designed to be **simple and fast** at the cost of some consistency guarantees:

- **Best-effort writes**: Multiple processes may write to the same key simultaneously (this is not true when using the serialized fetch approach)
- **Race conditions**: A newly-written entry might be immediately deleted by another process reading an expired entry
- **No transactions**: No locks or serialization for basic operations

These trade-offs are acceptable for use cases where:
- Occasional cache misses are fine
- The cached data can be recomputed if needed
- Performance and simplicity are more important than perfect consistency

If you need stronger consistency guarantees, consider using the serialized `fetch/*` functions or a different caching solution.

## License

MIT

## Links

- [GitHub](https://github.com/CrankWheel/kiss_cache)
- [Hex.pm](https://hex.pm/packages/kiss_cache)
