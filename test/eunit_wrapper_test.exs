defmodule EUnitWrapperTest do
  use ExUnit.Case

  test "run EUnit tests from Erlang modules" do
    # Run EUnit tests for the kiss_cache module
    result = :eunit.test(:kiss_cache, [:verbose])

    # EUnit returns 'ok' on success, 'error' on failure
    assert result == :ok, "EUnit tests failed"
  end
end
