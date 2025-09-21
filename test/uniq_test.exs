defmodule Uniq.Test do
  use ExUnit.Case, async: true
  import ExUnitProperties
  import Uniq.Test.Generators

  doctest Uniq.UUID

  alias Uniq.UUID

  setup do
    default = %{
      nil => "00000000-0000-0000-0000-000000000000",
      1 => "92fef5d6-c639-11eb-b8bc-0242ac130003",
      # generated in the :dns namespace, name "test"
      3 => "45a113ac-c7f2-30b0-90a5-a399ab912716",
      4 => "e5a4a3c3-45a7-4d5a-9809-e253a6ff8da2",
      # generated in the :dns namespace, name "test"
      5 => "4be0643f-1d98-573b-97cd-ca98a65347dd",
      6 => "1e7126af-f130-6780-adb4-8bbe7368fc2f",
      7 => "0182b66c-29e7-7ae8-b60e-4b669fe07c77",
      8 => "34e3992d-8a73-83de-9486-0f622063b665"
    }

    hex =
      default
      |> Enum.into(%{}, fn {k, v} -> {k, String.replace(v, "-", "")} end)

    raw =
      hex
      |> Enum.into(%{}, fn {k, v} -> {k, Base.decode16!(v, case: :lower)} end)

    urn =
      default
      |> Enum.into(%{}, fn {k, v} -> {k, "urn:uuid:" <> v} end)

    slug =
      raw
      |> Enum.into(%{}, fn {k, v} -> {k, Base.url_encode64(v, padding: false)} end)

    %{uuids: %{raw: raw, default: default, hex: hex, urn: urn, slug: slug}}
  end

  describe "parsing" do
    test "can handle nil uuid", %{uuids: uuids} do
      assert parse(nil, uuids)
    end

    test "can parse version 1", %{uuids: uuids} do
      assert parse(1, uuids)
    end

    test "can parse version 3", %{uuids: uuids} do
      assert parse(3, uuids)
    end

    test "can parse version 4", %{uuids: uuids} do
      assert parse(4, uuids)
    end

    test "can parse version 5", %{uuids: uuids} do
      assert parse(5, uuids)
    end

    test "can parse version 6", %{uuids: uuids} do
      assert parse(6, uuids)
    end

    test "can parse version 7", %{uuids: uuids} do
      assert parse(7, uuids)
    end

    test "can parse version 8", %{uuids: uuids} do
      assert parse(8, uuids)
    end

    property "can parse any 128-bit binary with valid version/variant values" do
      check all({version, variant, uuid} <- valid_uuid()) do
        assert {:ok, %UUID{version: ^version, variant: ^variant}} = UUID.parse(uuid)
      end
    end

    property "will reject any 128-bit binary with invalid version/variant values" do
      check all({_, _, uuid} <- invalid_uuid()) do
        assert {:error, _reason} = UUID.parse(uuid)
      end
    end

    property "can parse any 32-byte hex string which represents a valid uuid" do
      check all({version, variant, uuid} <- valid_uuid(:hex)) do
        assert {:ok, %UUID{version: ^version, variant: ^variant}} = UUID.parse(uuid)
      end
    end

    property "will reject any 32-byte hex string which represents an invalid uuid" do
      check all({_, _, uuid} <- invalid_uuid(:hex)) do
        assert {:error, _} = UUID.parse(uuid)
      end
    end

    property "can parse any 22-byte base64-encoded string which represents a valid uuid" do
      check all({version, variant, uuid} <- valid_uuid(:slug)) do
        assert {:ok, %UUID{version: ^version, variant: ^variant}} = UUID.parse(uuid)
      end
    end

    property "will reject any 22-byte base64-encoded string which represents an invalid uuid" do
      check all({_, _, uuid} <- invalid_uuid(:slug)) do
        assert {:error, _} = UUID.parse(uuid)
      end
    end
  end

  describe "formatting" do
    test "can format nil uuid", %{uuids: uuids} do
      assert format(nil, uuids)
    end

    test "can format version 1", %{uuids: uuids} do
      assert format(1, uuids)
    end

    test "can format version 3", %{uuids: uuids} do
      assert format(3, uuids)
    end

    test "can format version 4", %{uuids: uuids} do
      assert format(4, uuids)
    end

    test "can format version 5", %{uuids: uuids} do
      assert format(5, uuids)
    end

    test "can format version 6", %{uuids: uuids} do
      assert format(6, uuids)
    end

    test "can format version 7", %{uuids: uuids} do
      assert format(7, uuids)
    end

    test "can format version 8", %{uuids: uuids} do
      assert format(8, uuids)
    end
  end

  describe "generating" do
    test "can generate version 1" do
      default = UUID.uuid1()
      raw = UUID.uuid1(:raw)

      assert {:ok, %UUID{format: :default, version: 1}} = UUID.parse(default)
      assert {:ok, %UUID{format: :raw, version: 1}} = UUID.parse(raw)
    end

    test "can generate version 1 with random mac address" do
      {_, clock} = Uniq.Generator.next()
      node = UUID.random_mac_address()
      default = UUID.uuid1(clock, node, :default)
      raw = UUID.uuid1(clock, node, :raw)

      assert {:ok, %UUID{format: :default, version: 1}} = UUID.parse(default)
      assert {:ok, %UUID{format: :raw, version: 1}} = UUID.parse(raw)
    end

    test "can generate version 3", %{uuids: uuids} do
      namespace = <<0::128>>
      name = "test"

      default = UUID.uuid3(namespace, name)
      raw = UUID.uuid3(namespace, name, :raw)

      assert {:ok, %UUID{format: :default, version: 3}} = UUID.parse(default)
      assert {:ok, %UUID{format: :raw, version: 3}} = UUID.parse(raw)

      well_known_default = uuids[:default][3]
      well_known_raw = uuids[:raw][3]

      assert ^well_known_default = UUID.uuid3(:dns, name)
      assert ^well_known_raw = UUID.uuid3(:dns, name, :raw)
    end

    test "can generate version 4" do
      default = UUID.uuid4()
      raw = UUID.uuid4(:raw)

      assert {:ok, %UUID{format: :default, version: 4}} = UUID.parse(default)
      assert {:ok, %UUID{format: :raw, version: 4}} = UUID.parse(raw)
    end

    test "can generate version 5", %{uuids: uuids} do
      namespace = UUID.uuid1()
      name = "test"

      default = UUID.uuid5(namespace, name)
      raw = UUID.uuid5(namespace, name, :raw)

      assert {:ok, %UUID{format: :default, version: 5}} = UUID.parse(default)
      assert {:ok, %UUID{format: :raw, version: 5}} = UUID.parse(raw)

      well_known_default = uuids[:default][5]
      well_known_raw = uuids[:raw][5]

      assert ^well_known_default = UUID.uuid5(:dns, name)
      assert ^well_known_raw = UUID.uuid5(:dns, name, :raw)
    end

    test "can generate version 6" do
      default = UUID.uuid6()
      raw = UUID.uuid6(:raw)

      assert {:ok, %UUID{format: :default, version: 6}} = UUID.parse(default)
      assert {:ok, %UUID{format: :raw, version: 6}} = UUID.parse(raw)
    end

    test "can generate version 7" do
      default = UUID.uuid7()
      raw = UUID.uuid7(:raw)

      assert {:ok, %UUID{format: :default, version: 7}} = UUID.parse(default)
      assert {:ok, %UUID{format: :raw, version: 7}} = UUID.parse(raw)
    end
  end

  defp parse(version, uuids) do
    assert {:ok, %UUID{format: :raw, version: ^version}} = UUID.parse(uuids[:raw][version])

    assert {:ok, %UUID{format: :default, version: ^version}} =
             UUID.parse(uuids[:default][version])

    assert {:ok, %UUID{format: :hex, version: ^version}} = UUID.parse(uuids[:hex][version])
    assert {:ok, %UUID{format: :urn, version: ^version}} = UUID.parse(uuids[:urn][version])
    assert {:ok, %UUID{format: :slug, version: ^version}} = UUID.parse(uuids[:slug][version])

    true
  end

  defp format(version, uuids) do
    raw = uuids[:raw][version]
    default = uuids[:default][version]
    hex = uuids[:hex][version]
    urn = uuids[:urn][version]
    slug = uuids[:slug][version]

    assert ^default = UUID.to_string(raw)
    assert ^raw = UUID.to_string(raw, :raw)
    assert ^hex = UUID.to_string(raw, :hex) |> String.downcase()
    assert ^urn = UUID.to_string(raw, :urn)
    assert ^slug = UUID.to_string(raw, :slug)

    assert ^raw = UUID.to_string(default, :raw)
    assert ^default = UUID.to_string(slug, :default) |> String.downcase()

    true
  end
end
