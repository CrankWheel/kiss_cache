defmodule KissCache.TestHelpers do
  # Helper module for tests - provides utilities for testing

  # Echoes parameters to track function calls
  def test_echo(param) do
    cache = :kiss_cache_test_meta
    current = :kiss_cache.get(cache, :echoes, [])
    :kiss_cache.put(cache, :echoes, [param | current])
    param
  end

  # Get all echo calls made
  def get_echoes do
    cache = :kiss_cache_test_meta
    :kiss_cache.get(cache, :echoes, [])
  end

  # Clear echo history
  def clear_echoes do
    cache = :kiss_cache_test_meta
    :kiss_cache.delete(cache, :echoes)
  end

  # Function that returns error tuple
  def error_function do
    {:error, :something_went_wrong}
  end

  # Function that throws an exception
  def throwing_function do
    throw(:intentional_error)
  end

  # Expensive function that tracks call count
  def expensive_function(value) do
    cache = :kiss_cache_test_meta

    # Use atomic counter increment to avoid race conditions
    # Initialize if not exists
    case :ets.lookup(cache, :expensive_calls) do
      [] ->
        :ets.insert(cache, {:expensive_calls, 0, 0})
      _ ->
        :ok
    end

    # Atomically increment the counter (stored as value in position 3 of tuple)
    :ets.update_counter(cache, :expensive_calls, {3, 1})

    # Simulate some work
    Process.sleep(10)
    {:computed, value}
  end

  # Get expensive function call count
  def get_expensive_call_count do
    cache = :kiss_cache_test_meta
    case :ets.lookup(cache, :expensive_calls) do
      [{:expensive_calls, _expiry, count}] -> count
      [] -> 0
    end
  end

  # Reset expensive function call count
  def reset_expensive_call_count do
    cache = :kiss_cache_test_meta
    :kiss_cache.delete(cache, :expensive_calls)
  end
end
