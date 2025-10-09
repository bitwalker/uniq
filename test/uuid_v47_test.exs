defmodule Uniq.UUIDv47Test do
  use ExUnit.Case, async: true
  alias Uniq.{UUID, UUIDv47}

  doctest Uniq.UUIDv47

  describe "generate_key/0" do
    test "generates a valid 128-bit key" do
      {k0, k1} = UUIDv47.generate_key()

      assert is_integer(k0)
      assert is_integer(k1)
      assert k0 >= 0 and k0 <= 0xFFFFFFFFFFFFFFFF
      assert k1 >= 0 and k1 <= 0xFFFFFFFFFFFFFFFF
    end

    test "generates different keys on each call" do
      keys = Enum.map(1..10, fn _ -> UUIDv47.generate_key() end)
      unique_keys = Enum.uniq(keys)

      assert length(unique_keys) == 10
    end
  end

  describe "mask/2" do
    test "converts UUIDv7 to UUIDv4 facade" do
      uuid_v7 = UUID.uuid7(:raw)
      key = UUIDv47.generate_key()

      facade = UUIDv47.mask(uuid_v7, key)

      # Check version changed from 7 to 4
      <<_::48, version::4, _::76>> = facade
      assert version == 4

      # Check variant bits preserved (should be 0b10 for RFC 4122)
      <<_::64, variant::2, _::62>> = facade
      assert variant == 2
    end

    test "preserves random bits during masking" do
      # Create a UUIDv7 with known random bits
      timestamp = System.system_time(:millisecond)
      rand_a = 0xABC
      rand_b = 0x123456789ABCDEF0

      uuid_v7 = <<timestamp::big-unsigned-48, 7::4, rand_a::12, 2::2, rand_b::62>>
      key = UUIDv47.generate_key()

      facade = UUIDv47.mask(uuid_v7, key)

      # Extract random bits from facade
      <<_::48, 4::4, facade_rand_a::12, 2::2, facade_rand_b::62>> = facade

      # Random bits should be unchanged
      assert facade_rand_a == rand_a
      assert facade_rand_b == rand_b
    end

    test "produces deterministic output for same input" do
      uuid_v7 = UUID.uuid7(:raw)
      key = UUIDv47.generate_key()

      facade1 = UUIDv47.mask(uuid_v7, key)
      facade2 = UUIDv47.mask(uuid_v7, key)
      facade3 = UUIDv47.mask(uuid_v7, key)

      assert facade1 == facade2
      assert facade2 == facade3
    end

    test "different keys produce different facades" do
      uuid_v7 = UUID.uuid7(:raw)
      key1 = UUIDv47.generate_key()
      key2 = UUIDv47.generate_key()

      facade1 = UUIDv47.mask(uuid_v7, key1)
      facade2 = UUIDv47.mask(uuid_v7, key2)

      # Only timestamp should differ, random bits should be same
      <<ts1::48, _::80>> = facade1
      <<ts2::48, _::80>> = facade2

      assert ts1 != ts2
    end

    test "raises on non-v7 UUID" do
      uuid_v4 = UUID.uuid4(:raw)
      key = UUIDv47.generate_key()

      assert_raise ArgumentError, ~r/Expected a UUIDv7/, fn ->
        UUIDv47.mask(uuid_v4, key)
      end
    end
  end

  describe "unmask/2" do
    test "recovers original UUIDv7 from facade" do
      uuid_v7 = UUID.uuid7(:raw)
      key = UUIDv47.generate_key()

      facade = UUIDv47.mask(uuid_v7, key)
      recovered = UUIDv47.unmask(facade, key)

      assert recovered == uuid_v7
    end

    test "converts version 4 back to version 7" do
      uuid_v7 = UUID.uuid7(:raw)
      key = UUIDv47.generate_key()

      facade = UUIDv47.mask(uuid_v7, key)
      recovered = UUIDv47.unmask(facade, key)

      <<_::48, facade_version::4, _::76>> = facade
      <<_::48, recovered_version::4, _::76>> = recovered

      assert facade_version == 4
      assert recovered_version == 7
    end

    test "is deterministic" do
      uuid_v7 = UUID.uuid7(:raw)
      key = UUIDv47.generate_key()

      facade = UUIDv47.mask(uuid_v7, key)
      recovered1 = UUIDv47.unmask(facade, key)
      recovered2 = UUIDv47.unmask(facade, key)
      recovered3 = UUIDv47.unmask(facade, key)

      assert recovered1 == recovered2
      assert recovered2 == recovered3
    end

    test "wrong key produces different UUID" do
      uuid_v7 = UUID.uuid7(:raw)
      key1 = UUIDv47.generate_key()
      key2 = UUIDv47.generate_key()

      facade = UUIDv47.mask(uuid_v7, key1)
      recovered_wrong = UUIDv47.unmask(facade, key2)

      assert recovered_wrong != uuid_v7
    end

    test "raises on non-v4 UUID" do
      uuid_v7 = UUID.uuid7(:raw)
      key = UUIDv47.generate_key()

      assert_raise ArgumentError, ~r/Expected a UUIDv4 facade/, fn ->
        UUIDv47.unmask(uuid_v7, key)
      end
    end
  end

  describe "round-trip operations" do
    test "mask then unmask recovers original UUID" do
      key = UUIDv47.generate_key()

      for _ <- 1..100 do
        uuid_v7 = UUID.uuid7(:raw)
        facade = UUIDv47.mask(uuid_v7, key)
        recovered = UUIDv47.unmask(facade, key)

        assert recovered == uuid_v7
      end
    end

    test "multiple mask/unmask cycles preserve UUID" do
      uuid_v7 = UUID.uuid7(:raw)
      key = UUIDv47.generate_key()

      result =
        Enum.reduce(1..10, uuid_v7, fn _, uuid ->
          facade = UUIDv47.mask(uuid, key)
          UUIDv47.unmask(facade, key)
        end)

      assert result == uuid_v7
    end

    test "different UUIDs produce different facades" do
      key = UUIDv47.generate_key()

      facades =
        Enum.map(1..100, fn _ ->
          UUID.uuid7(:raw)
          |> UUIDv47.mask(key)
        end)

      unique_facades = Enum.uniq(facades)
      assert length(unique_facades) == 100
    end
  end

  describe "generate/2" do
    test "generates a valid UUID facade" do
      key = UUIDv47.generate_key()
      facade = UUIDv47.generate(key, :raw)

      assert byte_size(facade) == 16
      <<_::48, version::4, _::12, variant::2, _::62>> = facade
      assert version == 4
      assert variant == 2
    end

    test "generates facade in default format" do
      key = UUIDv47.generate_key()
      facade = UUIDv47.generate(key)

      assert is_binary(facade)
      assert byte_size(facade) == 36
      assert facade =~ ~r/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/
    end

    test "generates facade in hex format" do
      key = UUIDv47.generate_key()
      facade = UUIDv47.generate(key, :hex)

      assert is_binary(facade)
      assert byte_size(facade) == 32
      # Accept either uppercase or lowercase hex
      assert String.match?(facade, ~r/^[0-9a-fA-F]{32}$/)
    end

    test "generates unique facades" do
      key = UUIDv47.generate_key()
      facades = Enum.map(1..100, fn _ -> UUIDv47.generate(key) end)
      unique_facades = Enum.uniq(facades)

      assert length(unique_facades) == 100
    end
  end

  describe "v7?/1" do
    test "returns true for valid UUIDv7" do
      uuid_v7 = UUID.uuid7(:raw)
      assert UUIDv47.v7?(uuid_v7)
    end

    test "returns false for UUIDv4" do
      uuid_v4 = UUID.uuid4(:raw)
      refute UUIDv47.v7?(uuid_v4)
    end

    test "returns false for UUIDv1" do
      uuid_v1 = UUID.uuid1(:raw)
      refute UUIDv47.v7?(uuid_v1)
    end

    test "returns false for masked facade" do
      uuid_v7 = UUID.uuid7(:raw)
      key = UUIDv47.generate_key()
      facade = UUIDv47.mask(uuid_v7, key)

      assert UUIDv47.v7?(uuid_v7)
      refute UUIDv47.v7?(facade)
    end
  end

  describe "facade?/1" do
    test "returns true for valid v4 facade" do
      uuid_v7 = UUID.uuid7(:raw)
      key = UUIDv47.generate_key()
      facade = UUIDv47.mask(uuid_v7, key)

      assert UUIDv47.facade?(facade)
    end

    test "returns false for UUIDv7" do
      uuid_v7 = UUID.uuid7(:raw)
      refute UUIDv47.facade?(uuid_v7)
    end

    test "returns false for UUIDv1" do
      uuid_v1 = UUID.uuid1(:raw)
      refute UUIDv47.facade?(uuid_v1)
    end

    test "returns true for any valid UUIDv4 with correct variant" do
      # UUIDv47 facades are indistinguishable from regular v4 UUIDs
      uuid_v4 = UUID.uuid4(:raw)
      assert UUIDv47.facade?(uuid_v4)
    end
  end

  describe "timestamp preservation" do
    test "masked timestamp can be recovered" do
      # Use a known timestamp
      timestamp = 1_700_000_000_000
      rand_a = 0x123
      rand_b = 0x3FEDCBA9876543210

      uuid_v7 = <<timestamp::big-unsigned-48, 7::4, rand_a::12, 2::2, rand_b::62>>
      key = UUIDv47.generate_key()

      facade = UUIDv47.mask(uuid_v7, key)
      recovered = UUIDv47.unmask(facade, key)

      <<recovered_ts::big-unsigned-48, 7::4, _::76>> = recovered
      assert recovered_ts == timestamp
    end

    test "timestamp is obfuscated in facade" do
      # Use a known timestamp
      timestamp = 1_700_000_000_000
      rand_a = 0x123
      rand_b = 0x3FEDCBA9876543210

      uuid_v7 = <<timestamp::big-unsigned-48, 7::4, rand_a::12, 2::2, rand_b::62>>
      key = UUIDv47.generate_key()

      facade = UUIDv47.mask(uuid_v7, key)

      <<facade_ts::big-unsigned-48, 4::4, _::76>> = facade

      # Facade timestamp should be different from original
      assert facade_ts != timestamp
    end
  end

  describe "security properties" do
    test "same UUID with different keys produces different facades" do
      uuid_v7 = UUID.uuid7(:raw)

      facades =
        Enum.map(1..100, fn _ ->
          key = UUIDv47.generate_key()
          UUIDv47.mask(uuid_v7, key)
        end)

      unique_facades = Enum.uniq(facades)
      assert length(unique_facades) == 100
    end

    test "cannot recover UUID without correct key" do
      uuid_v7 = UUID.uuid7(:raw)
      correct_key = UUIDv47.generate_key()

      facade = UUIDv47.mask(uuid_v7, correct_key)

      # Try 10 wrong keys
      failed_recoveries =
        Enum.map(1..10, fn _ ->
          wrong_key = UUIDv47.generate_key()
          UUIDv47.unmask(facade, wrong_key)
        end)

      # None should match the original
      assert Enum.all?(failed_recoveries, fn recovered -> recovered != uuid_v7 end)
    end
  end
end
