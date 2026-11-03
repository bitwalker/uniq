defmodule Uniq.UUIDv47Test do
  use ExUnit.Case, async: true
  alias Uniq.{UUID, UUIDv47}

  doctest Uniq.UUIDv47

  @key {0x0123456789ABCDEF, 0xFEDCBA9876543210}
  @uuid <<0::48, 7::4, 0::12, 2::2, 0::62>>

  test "matches upstream C facade vectors in both directions" do
    rows =
      Path.join([__DIR__, "fixtures", "uuidv47", "uuidv47-vectors.tsv"])
      |> File.read!()
      |> String.split("\n", trim: true)
      |> tl()

    for row <- rows do
      [k0, k1, v7, facade | _] = String.split(row, "\t")
      key = {String.to_integer(k0, 16), String.to_integer(k1, 16)}
      v7 = UUID.string_to_binary!(v7)
      facade = UUID.string_to_binary!(facade)
      assert UUIDv47.mask(v7, key) == facade
      assert UUIDv47.unmask(facade, key) == v7
    end
  end

  test "preserves random bits and reverses the version transition" do
    for timestamp <- [0, 1, 0xFFFFFFFFFFFF],
        rand_a <- [0, 0xABC, 0xFFF],
        rand_b <- [0, 0x123456789ABCDEF, 0x3FFFFFFFFFFFFFFF] do
      uuid = <<timestamp::48, 7::4, rand_a::12, 2::2, rand_b::62>>
      facade = UUIDv47.mask(uuid, @key)
      assert <<_::48, 4::4, ^rand_a::12, 2::2, ^rand_b::62>> = facade
      assert UUIDv47.unmask(facade, @key) == uuid
    end
  end

  test "generates facades in every supported format" do
    for format <- [:raw, :default, :hex, :urn, :slug] do
      assert {:ok, %UUID{version: 4, variant: <<2::2>>, bytes: raw}} =
               @key |> UUIDv47.generate(format) |> UUID.parse()

      assert UUIDv47.v7?(UUIDv47.unmask(raw, @key))
    end
  end

  test "generated key words fit the unsigned 64-bit contract" do
    {k0, k1} = key = UUIDv47.generate_key()
    assert k0 in 0..0xFFFFFFFFFFFFFFFF
    assert k1 in 0..0xFFFFFFFFFFFFFFFF
    assert @uuid |> UUIDv47.mask(key) |> UUIDv47.unmask(key) == @uuid
  end

  test "rejects reserved variants and wrong versions instead of repairing them" do
    for version <- [1, 4, 6, 7, 8], variant <- 0..3 do
      uuid = <<0::48, version::4, 0::12, variant::2, 0::62>>

      unless version == 7 and variant == 2 do
        refute UUIDv47.v7?(uuid)
        assert_raise ArgumentError, fn -> UUIDv47.mask(uuid, @key) end
      end

      unless version == 4 and variant == 2 do
        refute UUIDv47.facade?(uuid)
        assert_raise ArgumentError, fn -> UUIDv47.unmask(uuid, @key) end
      end
    end
  end

  test "rejects malformed UUIDs and out-of-range keys" do
    for uuid <- [nil, "not a UUID", <<0::120>>, <<0::136>>, <<0::127>>] do
      assert_raise ArgumentError, fn -> UUIDv47.mask(uuid, @key) end
      assert_raise ArgumentError, fn -> UUIDv47.unmask(uuid, @key) end
    end

    for key <- [
          nil,
          {0},
          {-1, 0},
          {0, -1},
          {0x1_0000_0000_0000_0000, 0},
          {0, 0x1_0000_0000_0000_0000},
          {0, 1.0}
        ] do
      assert_raise ArgumentError, fn -> UUIDv47.mask(@uuid, key) end

      assert_raise ArgumentError, fn ->
        UUIDv47.unmask(<<0::48, 4::4, 0::12, 2::2, 0::62>>, key)
      end
    end

    assert_raise ArgumentError, fn -> UUIDv47.generate(@key, :invalid) end
  end

  test "shape checks do not authenticate facades or detect the wrong key" do
    facade = UUIDv47.mask(@uuid, @key)
    wrong = UUIDv47.unmask(facade, {0, 0})
    assert wrong != @uuid
    assert UUIDv47.v7?(wrong)
    assert UUIDv47.facade?(UUID.uuid4(:raw))
  end
end
