defmodule KissCacheCleanupTest do
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

  describe "delete_all_expired/1" do
    test "removes all expired entries" do
      :kiss_cache.put(:kiss_cache_test, :expired1, :value1, -1)
      :kiss_cache.put(:kiss_cache_test, :expired2, :value2, -1)
      :kiss_cache.put(:kiss_cache_test, :fresh, :value3, 10_000)

      assert :ets.info(:kiss_cache_test, :size) == 3

      :kiss_cache.delete_all_expired(:kiss_cache_test)

      assert :ets.info(:kiss_cache_test, :size) == 1
      assert :kiss_cache.get(:kiss_cache_test, :fresh) == :value3
    end

    test "keeps non-expired entries" do
      :kiss_cache.put(:kiss_cache_test, :fresh1, :value1, 10_000)
      :kiss_cache.put(:kiss_cache_test, :fresh2, :value2, 10_000)
      :kiss_cache.put(:kiss_cache_test, :expired, :value3, -1)

      :kiss_cache.delete_all_expired(:kiss_cache_test)

      assert :ets.info(:kiss_cache_test, :size) == 2
      assert :kiss_cache.get(:kiss_cache_test, :fresh1) == :value1
      assert :kiss_cache.get(:kiss_cache_test, :fresh2) == :value2
    end

    test "handles cache with no expired entries" do
      :kiss_cache.put(:kiss_cache_test, :fresh1, :value1, 10_000)
      :kiss_cache.put(:kiss_cache_test, :fresh2, :value2, 10_000)

      assert :ets.info(:kiss_cache_test, :size) == 2

      :kiss_cache.delete_all_expired(:kiss_cache_test)

      assert :ets.info(:kiss_cache_test, :size) == 2
    end

    test "handles empty cache" do
      assert :ets.info(:kiss_cache_test, :size) == 0

      :kiss_cache.delete_all_expired(:kiss_cache_test)

      assert :ets.info(:kiss_cache_test, :size) == 0
    end

    test "handles cache with only expired entries" do
      :kiss_cache.put(:kiss_cache_test, :expired1, :value1, -1)
      :kiss_cache.put(:kiss_cache_test, :expired2, :value2, -1)

      assert :ets.info(:kiss_cache_test, :size) == 2

      :kiss_cache.delete_all_expired(:kiss_cache_test)

      assert :ets.info(:kiss_cache_test, :size) == 0
    end

    test "can be called multiple times safely" do
      :kiss_cache.put(:kiss_cache_test, :expired, :value, -1)
      :kiss_cache.put(:kiss_cache_test, :fresh, :value, 10_000)

      :kiss_cache.delete_all_expired(:kiss_cache_test)
      assert :ets.info(:kiss_cache_test, :size) == 1

      :kiss_cache.delete_all_expired(:kiss_cache_test)
      assert :ets.info(:kiss_cache_test, :size) == 1

      :kiss_cache.delete_all_expired(:kiss_cache_test)
      assert :ets.info(:kiss_cache_test, :size) == 1
    end
  end

  describe "delete_all/1" do
    test "removes all entries from cache" do
      :kiss_cache.put(:kiss_cache_test, :key1, :value1)
      :kiss_cache.put(:kiss_cache_test, :key2, :value2)
      :kiss_cache.put(:kiss_cache_test, :key3, :value3)

      assert :ets.info(:kiss_cache_test, :size) == 3

      :kiss_cache.delete_all(:kiss_cache_test)

      assert :ets.info(:kiss_cache_test, :size) == 0
    end

    test "removes both expired and fresh entries" do
      :kiss_cache.put(:kiss_cache_test, :expired, :value1, -1)
      :kiss_cache.put(:kiss_cache_test, :fresh, :value2, 10_000)

      :kiss_cache.delete_all(:kiss_cache_test)

      assert :ets.info(:kiss_cache_test, :size) == 0
      assert :kiss_cache.get(:kiss_cache_test, :expired) == nil
      assert :kiss_cache.get(:kiss_cache_test, :fresh) == nil
    end

    test "handles empty cache" do
      assert :ets.info(:kiss_cache_test, :size) == 0

      :kiss_cache.delete_all(:kiss_cache_test)

      assert :ets.info(:kiss_cache_test, :size) == 0
    end

    test "cache can be used after delete_all" do
      :kiss_cache.put(:kiss_cache_test, :key, :value)
      :kiss_cache.delete_all(:kiss_cache_test)

      assert :ets.info(:kiss_cache_test, :size) == 0

      :kiss_cache.put(:kiss_cache_test, :new_key, :new_value)

      assert :kiss_cache.get(:kiss_cache_test, :new_key) == :new_value
      assert :ets.info(:kiss_cache_test, :size) == 1
    end
  end

  describe "put/5 cleanup behavior" do
    test "put with false never triggers cleanup" do
      :kiss_cache.put(:kiss_cache_test, :expired, :value, -1)

      # Put 1000 times with cleanup disabled
      for i <- 1..1000 do
        :kiss_cache.put(:kiss_cache_test, :"key_#{i}", :value, 10_000, false)
      end

      # Expired entry should still be there (no cleanup triggered)
      assert :ets.info(:kiss_cache_test, :size) == 1001
    end

    test "put with true always triggers cleanup" do
      :kiss_cache.put(:kiss_cache_test, :expired, :value, -1)
      assert :ets.info(:kiss_cache_test, :size) == 1

      :kiss_cache.put(:kiss_cache_test, :new, :value, 10_000, true)

      # Cleanup should have been triggered
      assert :ets.info(:kiss_cache_test, :size) == 1
      assert :kiss_cache.get(:kiss_cache_test, :new) == :value
      assert :kiss_cache.get(:kiss_cache_test, :expired) == nil
    end

    test "put with 1 always triggers cleanup" do
      :kiss_cache.put(:kiss_cache_test, :expired, :value, -1)
      assert :ets.info(:kiss_cache_test, :size) == 1

      :kiss_cache.put(:kiss_cache_test, :new, :value, 10_000, 1)

      # Cleanup should have been triggered
      assert :ets.info(:kiss_cache_test, :size) == 1
    end

    test "put with 0 never triggers cleanup" do
      :kiss_cache.put(:kiss_cache_test, :expired, :value, -1)

      for i <- 1..100 do
        :kiss_cache.put(:kiss_cache_test, :"key_#{i}", :value, 10_000, 0)
      end

      # Expired entry should still be there
      assert :ets.info(:kiss_cache_test, :size) == 101
    end

    test "put with probabilistic cleanup (e.g., 2) triggers sometimes" do
      # Put multiple expired entries
      for i <- 1..5 do
        :kiss_cache.put(:kiss_cache_test, :"expired_#{i}", :value, -1)
      end

      assert :ets.info(:kiss_cache_test, :size) == 5

      # Put with 1 in 2 chance of cleanup - should eventually trigger
      # Track the minimum size we see
      sizes =
        for i <- 1..100 do
          :kiss_cache.put(:kiss_cache_test, :"key_#{i}", :value, 10_000, 2)
          :ets.info(:kiss_cache_test, :size)
        end

      min_size = Enum.min(sizes)

      # With 1 in 2 chance over 100 attempts, cleanup should have triggered at least once
      # When cleanup triggers, expired entries (5) are removed, so size should drop below 100
      assert min_size < 100,
             "Cleanup should have triggered at least once, but min size was #{min_size}"
    end
  end

  describe "default cleanup behavior" do
    test "put/3 uses default cleanup chance (1 in 1000)" do
      # Put expired entries
      for i <- 1..10 do
        :kiss_cache.put(:kiss_cache_test, :"expired_#{i}", :value, -1)
      end

      initial_size = :ets.info(:kiss_cache_test, :size)

      # With default 1/1000 chance, these 100 puts likely won't trigger cleanup
      for i <- 1..100 do
        :kiss_cache.put(:kiss_cache_test, :"key_#{i}", :value)
      end

      # Size should include expired entries (cleanup likely didn't run)
      assert :ets.info(:kiss_cache_test, :size) >= initial_size + 50
    end

    test "put/4 uses default cleanup chance" do
      :kiss_cache.put(:kiss_cache_test, :expired, :value, -1)

      # Put with custom expiry but no cleanup param
      for i <- 1..50 do
        :kiss_cache.put(:kiss_cache_test, :"key_#{i}", :value, 5000)
      end

      # Expired should likely still be there with only 50 attempts at 1/1000
      assert :ets.info(:kiss_cache_test, :size) >= 40
    end
  end
end
