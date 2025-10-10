defmodule Uniq.SipHashTest do
  use ExUnit.Case, async: true
  alias Uniq.SipHash

  doctest Uniq.SipHash

  describe "hash/2" do
    # Property-based tests for SipHash-2-4
    # The UUIDv47 implementation relies on these properties

    test "handles empty message" do
      key = {0x0706050403020100, 0x0f0e0d0c0b0a0908}
      message = <<>>

      # Should produce a 64-bit hash
      hash = SipHash.hash(key, message)
      assert is_integer(hash)
      assert hash >= 0 and hash <= 0xFFFFFFFFFFFFFFFF
    end

    test "handles messages of various lengths" do
      key = {0x0706050403020100, 0x0f0e0d0c0b0a0908}

      # Test various message lengths
      for len <- 1..20 do
        message = :crypto.strong_rand_bytes(len)
        hash = SipHash.hash(key, message)

        assert is_integer(hash)
        assert hash >= 0 and hash <= 0xFFFFFFFFFFFFFFFF
      end
    end

    test "handles 10-byte messages (UUIDv47 size)" do
      key = {0x0706050403020100, 0x0f0e0d0c0b0a0908}
      message = <<0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09>>

      hash = SipHash.hash(key, message)
      assert is_integer(hash)
      assert hash >= 0 and hash <= 0xFFFFFFFFFFFFFFFF
    end

    test "produces deterministic output" do
      key = {0x0123456789abcdef, 0xfedcba9876543210}
      message = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>

      hash1 = SipHash.hash(key, message)
      hash2 = SipHash.hash(key, message)
      hash3 = SipHash.hash(key, message)

      assert hash1 == hash2
      assert hash2 == hash3
    end

    test "different keys produce different hashes" do
      key1 = {0x0123456789abcdef, 0xfedcba9876543210}
      key2 = {0xfedcba9876543210, 0x0123456789abcdef}
      message = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>

      hash1 = SipHash.hash(key1, message)
      hash2 = SipHash.hash(key2, message)

      assert hash1 != hash2
    end

    test "different messages produce different hashes" do
      key = {0x0123456789abcdef, 0xfedcba9876543210}
      message1 = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>
      message2 = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 11>>

      hash1 = SipHash.hash(key, message1)
      hash2 = SipHash.hash(key, message2)

      assert hash1 != hash2
    end
  end
end
