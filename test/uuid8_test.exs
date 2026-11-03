defmodule Uniq.UUID8Test do
  use ExUnit.Case, async: true

  alias Uniq.UUID

  @formats [:raw, :default, :hex, :urn, :slug]
  @raw <<0, 0, 0, 0, 0, 1, 0x80, 2, 0x80, 3, 0, 0, 0, 0, 0, 4>>

  test "parses opaque v8 payloads in every format without inventing metadata" do
    for format <- @formats do
      encoded = UUID.format(@raw, format)

      assert {:ok,
              %UUID{
                version: 8,
                variant: <<2::2>>,
                bytes: @raw,
                time: nil,
                seq: nil,
                node: nil
              } = info} = UUID.parse(encoded)

      assert UUID.to_string(info, :raw) == @raw
      assert UUID.compare(info, encoded) == :eq
      assert UUID.valid?(encoded)
    end
  end

  test "rejects reserved variants for v8 with an invalid-variant error" do
    for variant <- [0, 1, 2, 3, 6, 7] do
      raw = <<1::48, 8::4, 2::12, variant::3, 4::61>>
      assert {:error, {:invalid_variant, _}} = UUID.parse(raw)
      refute UUID.valid?(raw)
    end
  end

  test "unknown versions remain invalid while nil retains its special case" do
    for version <- [0, 2, 9, 10, 11, 12, 13, 14, 15] do
      assert {:error, {:unknown_version, ^version}} =
               UUID.parse(<<1::48, version::4, 2::12, 2::2, 4::62>>)
    end

    assert UUID.valid?(<<0::128>>)
    assert UUID.valid?(<<0::128>>, strict: true)
  end

  test "strict validation consistently excludes post-RFC4122 versions" do
    for version <- [6, 7, 8] do
      raw = <<1::48, version::4, 2::12, 2::2, 4::62>>
      assert UUID.valid?(raw)
      refute UUID.valid?(raw, strict: true)
    end

    for version <- [1, 3, 4, 5] do
      assert UUID.valid?(<<1::48, version::4, 2::12, 2::2, 4::62>>, strict: true)
    end
  end

  test "Ecto accepts supplied v8 values but does not offer v8 autogeneration" do
    params = UUID.init(schema: __MODULE__, field: :id)
    assert {:ok, uuid} = UUID.cast(@raw, params)
    assert {:ok, @raw} = UUID.dump(uuid, nil, params)

    assert_raise ArgumentError, fn ->
      UUID.init(schema: __MODULE__, field: :id, version: 8)
    end
  end
end
