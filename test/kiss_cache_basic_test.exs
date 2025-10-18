defmodule KissCacheBasicTest do
  use ExUnit.Case, async: false

  setup do
    # Clean up any existing test caches
    ETSHelpers.safe_delete_table(:kiss_cache_test)
    ETSHelpers.safe_delete_table(:kiss_cache_test_meta)

    # Start fresh caches
    :kiss_cache.start_cache(:kiss_cache_test)
    :kiss_cache.start_cache(:kiss_cache_test_meta)

    KissCache.TestHelpers.clear_echoes()

    :ok
  end

  describe "start_cache/1" do
    test "creates a new cache table" do
      cache = :test_cache_creation
      ETSHelpers.safe_delete_table(cache)

      result = :kiss_cache.start_cache(cache)

      assert result == cache
      assert :ets.info(cache) != :undefined

      # Clean up
      :ets.delete(cache)
    end

    test "cache is accessible after creation" do
      cache = :test_cache_accessible
      ETSHelpers.safe_delete_table(cache)

      :kiss_cache.start_cache(cache)
      :kiss_cache.put(cache, :test_key, :test_value)

      assert :kiss_cache.get(cache, :test_key) == :test_value

      # Clean up
      :ets.delete(cache)
    end
  end

  describe "put/3 and get/2" do
    test "stores and retrieves a value with default expiry" do
      :kiss_cache.put(:kiss_cache_test, :hello, :world)

      assert :kiss_cache.get(:kiss_cache_test, :hello) == :world
    end

    test "returns nil for non-existent key" do
      assert :kiss_cache.get(:kiss_cache_test, :nonexistent) == nil
    end

    test "stores different types of values" do
      :kiss_cache.put(:kiss_cache_test, :atom_key, :atom_value)
      :kiss_cache.put(:kiss_cache_test, "string_key", "string_value")
      :kiss_cache.put(:kiss_cache_test, :tuple_key, {:tuple, :value})
      :kiss_cache.put(:kiss_cache_test, :list_key, [1, 2, 3])
      :kiss_cache.put(:kiss_cache_test, :map_key, %{foo: :bar})

      assert :kiss_cache.get(:kiss_cache_test, :atom_key) == :atom_value
      assert :kiss_cache.get(:kiss_cache_test, "string_key") == "string_value"
      assert :kiss_cache.get(:kiss_cache_test, :tuple_key) == {:tuple, :value}
      assert :kiss_cache.get(:kiss_cache_test, :list_key) == [1, 2, 3]
      assert :kiss_cache.get(:kiss_cache_test, :map_key) == %{foo: :bar}
    end

    test "stores large values" do
      large_binary = :crypto.strong_rand_bytes(100_000)
      :kiss_cache.put(:kiss_cache_test, :large_value, large_binary)

      assert :kiss_cache.get(:kiss_cache_test, :large_value) == large_binary
    end

    test "uses complex keys" do
      :kiss_cache.put(:kiss_cache_test, {:compound, :key}, :value1)
      :kiss_cache.put(:kiss_cache_test, {:compound, :key, :nested}, :value2)

      assert :kiss_cache.get(:kiss_cache_test, {:compound, :key}) == :value1
      assert :kiss_cache.get(:kiss_cache_test, {:compound, :key, :nested}) == :value2
    end
  end

  describe "get/3 with default" do
    test "returns default value when key does not exist" do
      assert :kiss_cache.get(:kiss_cache_test, :missing, :my_default) == :my_default
    end

    test "returns actual value when key exists" do
      :kiss_cache.put(:kiss_cache_test, :exists, :actual_value)

      assert :kiss_cache.get(:kiss_cache_test, :exists, :my_default) == :actual_value
    end

    test "returns default when value has expired" do
      :kiss_cache.put(:kiss_cache_test, :expired_key, :expired_value, -1)

      assert :kiss_cache.get(:kiss_cache_test, :expired_key, :my_default) == :my_default
    end
  end

  describe "put/4 with custom expiry" do
    test "stores value with custom expiry time" do
      :kiss_cache.put(:kiss_cache_test, :custom_expiry, :value, 60_000)

      assert :kiss_cache.get(:kiss_cache_test, :custom_expiry) == :value
    end

    test "value expires after specified time" do
      :kiss_cache.put(:kiss_cache_test, :short_expiry, :value, 50)

      # Should exist immediately
      assert :kiss_cache.get(:kiss_cache_test, :short_expiry) == :value

      # Wait for expiry
      Process.sleep(100)

      # Should be expired now
      assert :kiss_cache.get(:kiss_cache_test, :short_expiry) == nil
    end

    test "immediate expiry with 0ms" do
      :kiss_cache.put(:kiss_cache_test, :zero_expiry, :value, 0)

      # With 0ms expiry, the entry expires at current time
      # Need to wait a tiny bit to ensure it's expired
      Process.sleep(1)
      assert :kiss_cache.get(:kiss_cache_test, :zero_expiry) == nil
    end

    test "negative expiry creates expired entry" do
      :kiss_cache.put(:kiss_cache_test, :negative_expiry, :value, -1000)

      # Should be expired
      assert :kiss_cache.get(:kiss_cache_test, :negative_expiry) == nil
    end
  end

  describe "delete/2" do
    test "deletes an existing key" do
      :kiss_cache.put(:kiss_cache_test, :to_delete, :value)
      assert :kiss_cache.get(:kiss_cache_test, :to_delete) == :value

      :kiss_cache.delete(:kiss_cache_test, :to_delete)

      assert :kiss_cache.get(:kiss_cache_test, :to_delete) == nil
    end

    test "delete on non-existent key does not error" do
      assert :kiss_cache.delete(:kiss_cache_test, :does_not_exist) == true
    end

    test "can re-insert after delete" do
      :kiss_cache.put(:kiss_cache_test, :reinsert, :value1)
      :kiss_cache.delete(:kiss_cache_test, :reinsert)
      :kiss_cache.put(:kiss_cache_test, :reinsert, :value2)

      assert :kiss_cache.get(:kiss_cache_test, :reinsert) == :value2
    end
  end

  describe "overwriting values" do
    test "putting same key overwrites previous value" do
      :kiss_cache.put(:kiss_cache_test, :overwrite, :value1)
      assert :kiss_cache.get(:kiss_cache_test, :overwrite) == :value1

      :kiss_cache.put(:kiss_cache_test, :overwrite, :value2)
      assert :kiss_cache.get(:kiss_cache_test, :overwrite) == :value2
    end

    test "overwriting resets expiry timer" do
      :kiss_cache.put(:kiss_cache_test, :reset_expiry, :value1, 100)
      Process.sleep(50)

      # Overwrite with longer expiry
      :kiss_cache.put(:kiss_cache_test, :reset_expiry, :value2, 10_000)
      Process.sleep(100)

      # Should still exist with new value
      assert :kiss_cache.get(:kiss_cache_test, :reset_expiry) == :value2
    end
  end
end
