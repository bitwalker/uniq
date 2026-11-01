defmodule Uniq.UUIDConversionTest do
  use ExUnit.Case, async: true

  alias Uniq.UUID

  @uuid "018dd112-d7d4-72fb-9650-558ff08be388"
  @raw <<0x018DD112D7D472FB9650558FF08BE388::128>>
  @urn "urn:uuid:" <> @uuid

  test "decodes URNs and converts them to every supported format" do
    assert UUID.string_to_binary!(@urn) == @raw
    assert UUID.to_string(@urn) == @uuid
    assert UUID.to_string(@urn, :default) == @uuid
    assert UUID.to_string(@urn, :raw) == @raw
    assert UUID.to_string(@urn, :hex) == Base.encode16(@raw)
    assert UUID.to_string(@urn, :slug) == Base.url_encode64(@raw, padding: false)
    assert UUID.to_string(@urn, :urn) == @urn
    assert UUID.string_to_binary!("urn:uuid:" <> String.upcase(@uuid)) == @raw
    assert UUID.compare(@urn, @uuid) == :eq
  end

  test "rejects malformed URNs" do
    for urn <- [
          "urn:uuid:",
          "urn:uuid:" <> String.slice(@uuid, 0, 35),
          "urn:uuid:" <> @uuid <> "0",
          "urn:uuid:" <> String.replace(@uuid, "d", "z"),
          "urn:uuid:" <> @urn,
          "URN:UUID:" <> @uuid
        ] do
      assert_raise ArgumentError, fn -> UUID.string_to_binary!(urn) end
    end
  end

  test "Ecto callbacks accept URNs in the configured representation" do
    for format <- [:default, :raw, :hex, :urn, :slug] do
      params = UUID.init(schema: __MODULE__, field: :id, format: format, dump: format)
      expected = UUID.to_string(@raw, format)

      assert UUID.cast(@urn, params) == {:ok, expected}
      assert UUID.load(@urn, nil, params) == {:ok, expected}
      assert UUID.dump(@urn, nil, params) == {:ok, expected}
    end
  end
end
