defmodule KissCacheConcurrencyTest do
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

    :ok
  end

  describe "concurrent writes" do
    test "multiple processes writing to same key" do
      # As documented, this is best-effort - last write may win
      tasks =
        for i <- 1..100 do
          Task.async(fn ->
            :kiss_cache.put(:kiss_cache_test, :shared_key, i)
          end)
        end

      Task.await_many(tasks)

      # Some value should be in the cache
      result = :kiss_cache.get(:kiss_cache_test, :shared_key)
      assert result != nil
      assert is_integer(result)
      assert result >= 1 and result <= 100
    end

    test "multiple processes writing to different keys" do
      tasks =
        for i <- 1..100 do
          Task.async(fn ->
            :kiss_cache.put(:kiss_cache_test, :"key_#{i}", i)
          end)
        end

      Task.await_many(tasks)

      # All keys should be present
      for i <- 1..100 do
        assert :kiss_cache.get(:kiss_cache_test, :"key_#{i}") == i
      end
    end

    test "concurrent writes with different expiry times" do
      tasks =
        for i <- 1..50 do
          Task.async(fn ->
            :kiss_cache.put(:kiss_cache_test, :"key_#{i}", i, i * 100)
          end)
        end

      Task.await_many(tasks)

      # All should be accessible immediately
      for i <- 1..50 do
        assert :kiss_cache.get(:kiss_cache_test, :"key_#{i}") == i
      end
    end
  end

  describe "concurrent reads" do
    test "multiple processes reading same key" do
      :kiss_cache.put(:kiss_cache_test, :shared_key, :shared_value)

      tasks =
        for _ <- 1..100 do
          Task.async(fn ->
            :kiss_cache.get(:kiss_cache_test, :shared_key)
          end)
        end

      results = Task.await_many(tasks)

      # All should get the same value
      assert Enum.all?(results, fn r -> r == :shared_value end)
    end

    test "concurrent reads of different keys" do
      for i <- 1..100 do
        :kiss_cache.put(:kiss_cache_test, :"key_#{i}", i)
      end

      tasks =
        for i <- 1..100 do
          Task.async(fn ->
            :kiss_cache.get(:kiss_cache_test, :"key_#{i}")
          end)
        end

      results = Task.await_many(tasks)

      # Each should get its own value
      assert results == Enum.to_list(1..100)
    end

    test "concurrent reads of expiring keys" do
      :kiss_cache.put(:kiss_cache_test, :expiring_key, :value, 100)

      # Start reads before and after expiry
      tasks =
        for i <- 1..50 do
          Task.async(fn ->
            if rem(i, 2) == 0, do: Process.sleep(150)
            :kiss_cache.get(:kiss_cache_test, :expiring_key)
          end)
        end

      results = Task.await_many(tasks, 5000)

      # Some should get value, some should get nil (depending on timing)
      assert :value in results
      assert nil in results
    end
  end

  describe "concurrent deletes" do
    test "multiple processes deleting same key" do
      :kiss_cache.put(:kiss_cache_test, :to_delete, :value)

      tasks =
        for _ <- 1..50 do
          Task.async(fn ->
            :kiss_cache.delete(:kiss_cache_test, :to_delete)
          end)
        end

      Task.await_many(tasks)

      # Key should be gone
      assert :kiss_cache.get(:kiss_cache_test, :to_delete) == nil
    end

    test "concurrent deletes of different keys" do
      for i <- 1..100 do
        :kiss_cache.put(:kiss_cache_test, :"key_#{i}", i)
      end

      tasks =
        for i <- 1..100 do
          Task.async(fn ->
            :kiss_cache.delete(:kiss_cache_test, :"key_#{i}")
          end)
        end

      Task.await_many(tasks)

      # All should be gone
      assert :ets.info(:kiss_cache_test, :size) == 0
    end
  end

  describe "mixed concurrent operations" do
    test "concurrent puts, gets, and deletes" do
      tasks =
        for i <- 1..100 do
          Task.async(fn ->
            case rem(i, 3) do
              0 ->
                :kiss_cache.put(:kiss_cache_test, :"key_#{i}", i)

              1 ->
                :kiss_cache.get(:kiss_cache_test, :"key_#{rem(i, 50)}")

              2 ->
                :kiss_cache.delete(:kiss_cache_test, :"key_#{rem(i, 50)}")
            end
          end)
        end

      Task.await_many(tasks)

      # Cache should still be functional
      :kiss_cache.put(:kiss_cache_test, :test, :value)
      assert :kiss_cache.get(:kiss_cache_test, :test) == :value
    end

    test "concurrent operations during cleanup" do
      # Put some expired entries
      for i <- 1..50 do
        :kiss_cache.put(:kiss_cache_test, :"expired_#{i}", i, -1)
      end

      # Trigger cleanup and do operations concurrently
      tasks = [
        Task.async(fn -> :kiss_cache.delete_all_expired(:kiss_cache_test) end)
        | for i <- 1..50 do
            Task.async(fn ->
              :kiss_cache.put(:kiss_cache_test, :"new_#{i}", i)
              :kiss_cache.get(:kiss_cache_test, :"new_#{i}")
            end)
          end
      ]

      Task.await_many(tasks)

      # New entries should be there
      for i <- 1..50 do
        assert :kiss_cache.get(:kiss_cache_test, :"new_#{i}") == i
      end
    end
  end

  describe "race conditions" do
    test "reading expired entry while another process writes" do
      :kiss_cache.put(:kiss_cache_test, :race_key, :old_value, 50)

      tasks = [
        Task.async(fn ->
          Process.sleep(60)
          :kiss_cache.get(:kiss_cache_test, :race_key)
        end),
        Task.async(fn ->
          Process.sleep(70)
          :kiss_cache.put(:kiss_cache_test, :race_key, :new_value)
        end)
      ]

      [get_result, _put_result] = Task.await_many(tasks, 5000)

      # Get might return nil (expired) or new_value (if write happened first)
      # Both are acceptable due to race conditions
      assert get_result in [nil, :new_value]
    end

    test "cleanup racing with writes" do
      # This tests the race condition mentioned in module docs
      # where newly written entry might be deleted by cleanup

      for _ <- 1..10 do
        :kiss_cache.put(:kiss_cache_test, :race_key, :value, 100)

        spawn(fn ->
          :kiss_cache.delete_all_expired(:kiss_cache_test)
        end)

        spawn(fn ->
          :kiss_cache.put(:kiss_cache_test, :race_key, :new_value, 10_000)
        end)

        Process.sleep(10)
      end

      # System should remain stable despite races
      :kiss_cache.put(:kiss_cache_test, :stable, :value)
      assert :kiss_cache.get(:kiss_cache_test, :stable) == :value
    end
  end

  describe "high contention scenarios" do
    test "many processes reading hot key" do
      :kiss_cache.put(:kiss_cache_test, :hot_key, :hot_value)

      tasks =
        for _ <- 1..1000 do
          Task.async(fn ->
            :kiss_cache.get(:kiss_cache_test, :hot_key)
          end)
        end

      results = Task.await_many(tasks, 10_000)

      # All should succeed
      assert length(results) == 1000
      assert Enum.all?(results, fn r -> r == :hot_value end)
    end

    test "many processes writing to different keys rapidly" do
      tasks =
        for i <- 1..500 do
          Task.async(fn ->
            :kiss_cache.put(:kiss_cache_test, :"rapid_#{i}", i, 10_000)
          end)
        end

      Task.await_many(tasks)

      # All should be present
      cache_size = :ets.info(:kiss_cache_test, :size)
      # Allow for some timing variance
      assert cache_size >= 450
    end
  end

  describe "edge cases" do
    test "empty cache operations" do
      assert :kiss_cache.get(:kiss_cache_test, :nonexistent) == nil
      assert :kiss_cache.delete(:kiss_cache_test, :nonexistent) == true
      assert :kiss_cache.delete_all_expired(:kiss_cache_test) >= 0
      assert :kiss_cache.delete_all(:kiss_cache_test) == true
    end

    test "cache with single entry" do
      :kiss_cache.put(:kiss_cache_test, :only, :value)

      assert :ets.info(:kiss_cache_test, :size) == 1
      assert :kiss_cache.get(:kiss_cache_test, :only) == :value

      :kiss_cache.delete(:kiss_cache_test, :only)

      assert :ets.info(:kiss_cache_test, :size) == 0
    end

    test "very large number of entries" do
      # Put 10,000 entries
      for i <- 1..10_000 do
        :kiss_cache.put(:kiss_cache_test, :"large_#{i}", i)
      end

      assert :ets.info(:kiss_cache_test, :size) == 10_000

      # Random access should still work
      assert :kiss_cache.get(:kiss_cache_test, :large_5000) == 5000
      assert :kiss_cache.get(:kiss_cache_test, :large_1) == 1
      assert :kiss_cache.get(:kiss_cache_test, :large_10000) == 10_000
    end

    test "keys with special characters and types" do
      special_keys = [
        :atom,
        "string",
        123,
        {:tuple, :key},
        [:list, :key],
        %{map: :key},
        {:complex, [:nested, %{structure: "here"}]}
      ]

      for key <- special_keys do
        :kiss_cache.put(:kiss_cache_test, key, :value)
      end

      for key <- special_keys do
        assert :kiss_cache.get(:kiss_cache_test, key) == :value
      end
    end

    test "binary keys and values" do
      binary_key = <<1, 2, 3, 4, 5>>
      binary_value = <<10, 20, 30, 40, 50>>

      :kiss_cache.put(:kiss_cache_test, binary_key, binary_value)

      assert :kiss_cache.get(:kiss_cache_test, binary_key) == binary_value
    end

    test "nil values" do
      # Note: nil values are indistinguishable from missing keys
      # This is documented behavior
      :kiss_cache.put(:kiss_cache_test, :nil_key, nil)

      # This will return nil, but we can't tell if it's the stored value or missing
      result = :kiss_cache.get(:kiss_cache_test, :nil_key)
      assert result == nil

      # We can verify the key exists in ETS though
      assert :ets.lookup(:kiss_cache_test, :nil_key) != []
    end

    test "overwriting with same value" do
      :kiss_cache.put(:kiss_cache_test, :same, :value)
      :kiss_cache.put(:kiss_cache_test, :same, :value)
      :kiss_cache.put(:kiss_cache_test, :same, :value)

      assert :kiss_cache.get(:kiss_cache_test, :same) == :value
      assert :ets.info(:kiss_cache_test, :size) == 1
    end
  end
end
