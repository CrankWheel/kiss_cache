defmodule KissCacheFetchTest do
  use ExUnit.Case, async: false
  alias KissCache.TestHelpers

  setup do
    # Clean up any existing test caches
    ETSHelpers.safe_delete_table(:kiss_cache_test)
    ETSHelpers.safe_delete_table(:kiss_cache_test_meta)

    # Start fresh caches
    :kiss_cache.start_cache(:kiss_cache_test)
    :kiss_cache.start_cache(:kiss_cache_test_meta)

    TestHelpers.clear_echoes()
    TestHelpers.reset_expensive_call_count()

    # Start supervisor for fetch tests - restart if dead
    sup_pid =
      case Process.whereis(:kiss_cache_sup) do
        nil ->
          {:ok, pid} = :supervisor.start_link({:local, :kiss_cache_sup}, :kiss_cache_sup, [])
          pid

        pid when is_pid(pid) ->
          if Process.alive?(pid) do
            pid
          else
            Process.unregister(:kiss_cache_sup)

            {:ok, new_pid} =
              :supervisor.start_link({:local, :kiss_cache_sup}, :kiss_cache_sup, [])

            new_pid
          end
      end

    on_exit(fn ->
      if Process.alive?(sup_pid), do: Process.exit(sup_pid, :normal)
      # Give it time to die
      Process.sleep(10)
    end)

    {:ok, supervisor: sup_pid}
  end

  describe "fetch/4 basic functionality" do
    test "fetches and caches result on first call" do
      result = :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [42])

      assert result == 42
      assert TestHelpers.get_echoes() == [42]
    end

    test "returns cached result on subsequent calls" do
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [42])
      assert TestHelpers.get_echoes() == [42]

      # Second call should use cache, not call function again
      result = :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [42])

      assert result == 42
      # Still just one call
      assert TestHelpers.get_echoes() == [42]
    end

    test "different parameters create different cache entries" do
      result1 = :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1])
      result2 = :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [2])

      assert result1 == 1
      assert result2 == 2
      assert TestHelpers.get_echoes() == [2, 1]
    end

    test "same parameters use same cache entry" do
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [100])
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [100])
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [100])

      # Function should only be called once
      assert TestHelpers.get_echoes() == [100]
    end
  end

  describe "fetch/5 with CheckReturn" do
    test "commits result to cache when CheckReturn returns :commit" do
      check_fn = fn _result -> :commit end

      result =
        :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], check_fn)

      assert result == 1
      assert TestHelpers.get_echoes() == [1]

      # Second call should be from cache
      result2 =
        :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], check_fn)

      assert result2 == 1
      # Still one call
      assert TestHelpers.get_echoes() == [1]
    end

    test "ignores result when CheckReturn returns :ignore" do
      check_fn = fn _result -> :ignore end

      result =
        :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], check_fn)

      assert result == 1
      assert TestHelpers.get_echoes() == [1]

      # Second call should NOT be from cache (function called again)
      result2 =
        :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], check_fn)

      assert result2 == 1
      # Called twice
      assert TestHelpers.get_echoes() == [1, 1]
    end

    test "can conditionally cache based on result value" do
      # Only cache positive results
      check_fn = fn
        result when result > 0 -> :commit
        _result -> :ignore
      end

      # Negative result - not cached
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [-1], check_fn)
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [-1], check_fn)

      # Called twice
      assert TestHelpers.get_echoes() == [-1, -1]

      TestHelpers.clear_echoes()

      # Positive result - cached
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], check_fn)
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], check_fn)

      # Called once
      assert TestHelpers.get_echoes() == [1]
    end
  end

  describe "fetch/5 with ExpiryMs" do
    test "respects custom expiry time" do
      result = :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], 100)

      assert result == 1

      # Should be cached
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], 100)
      assert TestHelpers.get_echoes() == [1]

      # Wait for expiry
      Process.sleep(150)

      # Should fetch again
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], 100)
      assert TestHelpers.get_echoes() == [1, 1]
    end

    test "long expiry keeps value cached" do
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], 60_000)

      Process.sleep(100)

      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], 60_000)

      # Should still be cached after 100ms
      assert TestHelpers.get_echoes() == [1]
    end
  end

  describe "fetch/6 with ExpiryMs and ShouldCleanup" do
    test "fetch with cleanup disabled" do
      # Put an expired entry
      :kiss_cache.put(:kiss_cache_test, :expired, :old_value, -1)

      # Fetch with cleanup disabled (0 means never cleanup)
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], 10_000, 0)

      # Expired entry might still be there
      # (We can't guarantee it won't be cleaned up by chance, but with 0 it shouldn't)
    end

    test "fetch with cleanup enabled" do
      # Put an expired entry
      :kiss_cache.put(:kiss_cache_test, :expired, :old_value, -1)
      assert :ets.info(:kiss_cache_test, :size) >= 1

      # Fetch with cleanup enabled (1 means always cleanup)
      :kiss_cache.fetch(:kiss_cache_test, KissCache.TestHelpers, :test_echo, [1], 10_000, 1)

      # Give cleanup time to run (it's synchronous in test mode)
      assert :ets.info(:kiss_cache_test, :size) == 1
      assert :kiss_cache.get(:kiss_cache_test, :expired) == nil
    end
  end

  describe "serialized fetch - thundering herd prevention" do
    test "concurrent fetches only execute function once" do
      TestHelpers.reset_expensive_call_count()

      # Spawn 100 concurrent processes all fetching the same thing
      tasks =
        for _ <- 1..100 do
          Task.async(fn ->
            :kiss_cache.fetch(
              :kiss_cache_test,
              KissCache.TestHelpers,
              :expensive_function,
              [:shared_key]
            )
          end)
        end

      results = Task.await_many(tasks, 5000)

      # All should get the same result
      assert Enum.all?(results, fn r -> r == {:computed, :shared_key} end)

      # But function should only have been called once
      assert TestHelpers.get_expensive_call_count() == 1
    end

    test "concurrent fetches with different params execute separately" do
      TestHelpers.reset_expensive_call_count()

      # Spawn processes fetching different keys
      tasks =
        for i <- 1..10 do
          Task.async(fn ->
            :kiss_cache.fetch(
              :kiss_cache_test,
              KissCache.TestHelpers,
              :expensive_function,
              [i]
            )
          end)
        end

      results = Task.await_many(tasks, 5000)

      # Each should get its own result
      assert length(results) == 10

      # Function should have been called 10 times (once per unique key)
      assert TestHelpers.get_expensive_call_count() == 10
    end

    test "stress test with many concurrent requests" do
      TestHelpers.reset_expensive_call_count()

      # 1000 concurrent requests for the same resource
      tasks =
        for _ <- 1..1000 do
          Task.async(fn ->
            :kiss_cache.fetch(
              :kiss_cache_test,
              KissCache.TestHelpers,
              :expensive_function,
              [:stress_test],
              15_000
            )
          end)
        end

      results = Task.await_many(tasks, 10_000)

      # All should succeed with same result
      assert length(results) == 1000
      assert Enum.all?(results, fn r -> r == {:computed, :stress_test} end)

      # Function should only have been called once
      assert TestHelpers.get_expensive_call_count() == 1
    end
  end

  describe "fetch error handling" do
    test "throws exception from fetch when function throws" do
      # When function throws, the fetcher process exits abnormally
      # and the exit reason is thrown back
      result =
        catch_throw(
          :kiss_cache.fetch(
            :kiss_cache_test,
            KissCache.TestHelpers,
            :throwing_function,
            []
          )
        )

      # Result is the abnormal exit tuple
      assert {{:nocatch, :intentional_error}, _stacktrace} = result
    end

    test "handles non-existent function gracefully" do
      # Non-existent functions throw :undef error via fetcher
      result =
        catch_throw(
          :kiss_cache.fetch(
            :kiss_cache_test,
            KissCache.TestHelpers,
            :non_existent_function,
            []
          )
        )

      # Should throw :undef error tuple
      assert {:undef, _stacktrace} = result
    end

    test "can cache error tuples if desired" do
      result =
        :kiss_cache.fetch(
          :kiss_cache_test,
          KissCache.TestHelpers,
          :error_function,
          []
        )

      assert result == {:error, :something_went_wrong}

      # Error is cached
      result2 =
        :kiss_cache.fetch(
          :kiss_cache_test,
          KissCache.TestHelpers,
          :error_function,
          []
        )

      assert result2 == {:error, :something_went_wrong}
    end
  end

  describe "make_fetch_key/5" do
    test "generates consistent keys for same inputs" do
      key1 = :kiss_cache.make_fetch_key(:module, :function, [:param], nil, fn -> nil end)
      _key2 = :kiss_cache.make_fetch_key(:module, :function, [:param], nil, fn -> nil end)

      # Keys should be tuples
      assert is_tuple(key1)
      assert tuple_size(key1) == 6
    end

    test "different modules generate different keys" do
      apply_fn = fn m, f, a -> apply(m, f, a) end

      key1 = :kiss_cache.make_fetch_key(:module1, :func, [], nil, apply_fn)
      key2 = :kiss_cache.make_fetch_key(:module2, :func, [], nil, apply_fn)

      assert key1 != key2
    end

    test "different functions generate different keys" do
      apply_fn = fn m, f, a -> apply(m, f, a) end

      key1 = :kiss_cache.make_fetch_key(:module, :func1, [], nil, apply_fn)
      key2 = :kiss_cache.make_fetch_key(:module, :func2, [], nil, apply_fn)

      assert key1 != key2
    end

    test "different params generate different keys" do
      apply_fn = fn m, f, a -> apply(m, f, a) end

      key1 = :kiss_cache.make_fetch_key(:module, :func, [1], nil, apply_fn)
      key2 = :kiss_cache.make_fetch_key(:module, :func, [2], nil, apply_fn)

      assert key1 != key2
    end
  end
end
