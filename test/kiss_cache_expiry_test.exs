defmodule KissCacheExpiryTest do
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

  describe "expiry behavior" do
    test "value expires after default 10 seconds" do
      :kiss_cache.put(:kiss_cache_test, :default_expiry, :value)

      # Should exist immediately
      assert :kiss_cache.get(:kiss_cache_test, :default_expiry) == :value

      # Fast-forward by checking internal ETS structure
      # In real scenarios, would wait 10+ seconds
      [{:default_expiry, expires_at, _value}] = :ets.lookup(:kiss_cache_test, :default_expiry)
      now = :os.system_time(:millisecond)

      # Should be set to expire ~10 seconds from now (allowing for some timing variance)
      assert expires_at > now + 9_000
      assert expires_at < now + 11_000
    end

    test "expired entries return nil on get" do
      :kiss_cache.put(:kiss_cache_test, :expires_soon, :value, 50)

      # Exists now
      assert :kiss_cache.get(:kiss_cache_test, :expires_soon) == :value

      # Wait for expiry
      Process.sleep(100)

      # Should be nil now
      assert :kiss_cache.get(:kiss_cache_test, :expires_soon) == nil
    end

    test "expired entries return default value on get/3" do
      :kiss_cache.put(:kiss_cache_test, :expires_soon, :value, 50)

      Process.sleep(100)

      assert :kiss_cache.get(:kiss_cache_test, :expires_soon, :my_default) == :my_default
    end

    test "non-expired entries remain accessible" do
      :kiss_cache.put(:kiss_cache_test, :long_lived, :value, 10_000)

      Process.sleep(100)

      # Should still be there
      assert :kiss_cache.get(:kiss_cache_test, :long_lived) == :value
    end

    test "multiple entries with different expiry times" do
      :kiss_cache.put(:kiss_cache_test, :expires_fast, :value1, 50)
      :kiss_cache.put(:kiss_cache_test, :expires_slow, :value2, 10_000)

      Process.sleep(100)

      # Fast one should be gone
      assert :kiss_cache.get(:kiss_cache_test, :expires_fast) == nil

      # Slow one should still be there
      assert :kiss_cache.get(:kiss_cache_test, :expires_slow) == :value2
    end
  end

  describe "lazy cleanup on read" do
    test "reading expired entry removes it from cache" do
      :kiss_cache.put(:kiss_cache_test, :lazy_cleanup, :value, 50)

      # Entry exists in ETS
      assert :ets.info(:kiss_cache_test, :size) == 1

      Process.sleep(100)

      # Reading it should return nil AND delete it
      assert :kiss_cache.get(:kiss_cache_test, :lazy_cleanup) == nil

      # Should be removed from ETS
      assert :ets.info(:kiss_cache_test, :size) == 0
    end

    test "lazy cleanup only affects expired entries" do
      :kiss_cache.put(:kiss_cache_test, :expired, :value1, 50)
      :kiss_cache.put(:kiss_cache_test, :fresh, :value2, 10_000)

      Process.sleep(100)

      # Reading expired entry removes it
      assert :kiss_cache.get(:kiss_cache_test, :expired) == nil

      # Fresh entry still there
      assert :kiss_cache.get(:kiss_cache_test, :fresh) == :value2
      assert :ets.info(:kiss_cache_test, :size) == 1
    end

    test "expired entries not accessed remain in ETS until read" do
      :kiss_cache.put(:kiss_cache_test, :never_read, :value, 50)
      :kiss_cache.put(:kiss_cache_test, :other, :value2, 10_000)

      assert :ets.info(:kiss_cache_test, :size) == 2

      Process.sleep(100)

      # Expired entry still in ETS (hasn't been read)
      assert :ets.info(:kiss_cache_test, :size) == 2

      # Reading it triggers cleanup
      assert :kiss_cache.get(:kiss_cache_test, :never_read) == nil
      assert :ets.info(:kiss_cache_test, :size) == 1
    end
  end

  describe "expiry edge cases" do
    test "zero millisecond expiry" do
      :kiss_cache.put(:kiss_cache_test, :zero, :value, 0)

      # With 0ms expiry, need to wait a bit for it to be expired
      Process.sleep(1)
      assert :kiss_cache.get(:kiss_cache_test, :zero) == nil
    end

    test "negative expiry milliseconds" do
      :kiss_cache.put(:kiss_cache_test, :negative, :value, -5000)

      # Should be expired
      assert :kiss_cache.get(:kiss_cache_test, :negative) == nil
    end

    test "very long expiry" do
      one_day_ms = 24 * 60 * 60 * 1000
      :kiss_cache.put(:kiss_cache_test, :long_term, :value, one_day_ms)

      # Should be accessible
      assert :kiss_cache.get(:kiss_cache_test, :long_term) == :value

      # Verify expiry is set far in the future
      [{:long_term, expires_at, _}] = :ets.lookup(:kiss_cache_test, :long_term)
      now = :os.system_time(:millisecond)

      assert expires_at > now + (one_day_ms - 1000)
    end

    test "boundary condition - entry expires during get" do
      # This tests the race condition mentioned in the module docs
      # Put with very short expiry
      :kiss_cache.put(:kiss_cache_test, :boundary, :value, 30)

      # Sleep just under the expiry
      Process.sleep(25)

      # Get might return value or nil depending on timing
      result = :kiss_cache.get(:kiss_cache_test, :boundary)
      assert result in [:value, nil]
    end
  end
end
