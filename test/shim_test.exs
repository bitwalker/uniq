defmodule Uniq.ShimTests do
  @moduledoc "These tests were lifted straight from elixir_uuid so we can ensure compatibility"
  use ExUnit.Case, async: true

  @info_tests_path Path.join([__DIR__, "fixtures", "elixir_uuid_info_tests.txt"])

  test "UUID.info/1 invalid argument type" do
    assert UUID.info(:not_a_uuid) == {:error, :invalid_format}
  end

  test "UUID.info/1 invalid UUID" do
    assert UUID.info("not_a_uuid") == {:error, :invalid_format}
  end

  test "UUID.info!/1 invalid argument type" do
    assert_raise(
      ArgumentError,
      "invalid uuid: :invalid_format",
      fn ->
        UUID.info!(:not_a_uuid)
      end
    )
  end

  test "UUID.info!/1 invalid UUID" do
    assert_raise(
      ArgumentError,
      "invalid uuid: :invalid_format",
      fn ->
        UUID.info!("not_a_uuid")
      end
    )
  end

  # Expand the lines in info_tests.txt into individual tests for the
  # UUID.info!/1 and UUID.info/1 functions, assuming the lines are:
  #   test name || expected output || input value
  for line <- File.stream!(@info_tests_path) do
    [name, expected, input] = line |> String.split("||") |> Enum.map(&String.trim/1)

    test "UUID.info!/1 #{name}" do
      {expected, []} = Code.eval_string(unquote(expected))
      result = UUID.info!(unquote(input))
      assert ^expected = result
    end

    test "UUID.info/1 #{name}" do
      {expected, []} = Code.eval_string(unquote(expected))
      {:ok, result} = UUID.info(unquote(input))
      assert ^expected = result
    end
  end
end
