defmodule Uniq.SipHashTest do
  use ExUnit.Case, async: true
  alias Uniq.SipHash

  doctest Uniq.SipHash

  test "matches every official SipHash-2-4 vector and extended length boundaries" do
    for file <- ["siphash-vectors.tsv", "siphash-extended.tsv"],
        row <- fixture(file) do
      [length, k0, k1, message, hash] = String.split(row, "\t")
      key = {String.to_integer(k0, 16), String.to_integer(k1, 16)}
      message = Base.decode16!(message, case: :mixed)
      expected = String.to_integer(hash, 16)
      assert SipHash.hash(key, message) == expected, "vector length #{length}"
    end
  end

  test "rejects malformed keys and non-binary messages" do
    for key <- [
          nil,
          {},
          {0},
          {0, 0, 0},
          {-1, 0},
          {0, -1},
          {0x1_0000_0000_0000_0000, 0},
          {0, 0x1_0000_0000_0000_0000},
          {1.0, 0}
        ] do
      assert_raise ArgumentError, fn -> SipHash.hash(key, <<0>>) end
    end

    assert_raise ArgumentError, fn -> SipHash.hash({0, 0}, [0]) end
  end

  defp fixture(file) do
    Path.join([__DIR__, "fixtures", "uuidv47", file])
    |> File.read!()
    |> String.split("\n", trim: true)
    |> tl()
  end
end
