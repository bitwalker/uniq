defmodule Uniq.SipHash do
  @moduledoc """
  SipHash-2-4 implementation for cryptographic hashing.

  SipHash is a fast, short-input PRF designed for hash tables and
  other applications where speed and security matter. This module
  implements the SipHash-2-4 variant, which uses 2 compression rounds
  and 4 finalization rounds.

  ## References

  * [SipHash Paper](https://131002.net/siphash/siphash.pdf)
  * [UUIDv47 Specification](https://github.com/stateless-me/uuidv47)
  """

  import Bitwise

  @type key :: {non_neg_integer, non_neg_integer}

  @doc """
  Computes SipHash-2-4 of the given message with the provided key.

  The key should be a tuple of two 64-bit integers `{k0, k1}`.
  Returns a 64-bit integer hash value.

  ## Examples

      iex> key = {0x0123456789abcdef, 0xfedcba9876543210}
      iex> message = <<1, 2, 3, 4, 5>>
      iex> hash1 = Uniq.SipHash.hash(key, message)
      iex> hash2 = Uniq.SipHash.hash(key, message)
      iex> hash1 == hash2
      true

  """
  @spec hash(key, binary) :: non_neg_integer
  def hash({k0, k1}, message) when is_integer(k0) and is_integer(k1) and is_binary(message) do
    # Initialize state
    v0 = bxor(k0, 0x736f6d6570736575)
    v1 = bxor(k1, 0x646f72616e646f6d)
    v2 = bxor(k0, 0x6c7967656e657261)
    v3 = bxor(k1, 0x7465646279746573)

    # Process message in 8-byte chunks
    {state, remaining} = process_blocks(message, {v0, v1, v2, v3})

    # Finalization
    finalize(state, remaining, byte_size(message))
  end

  # Process 8-byte blocks
  defp process_blocks(<<block::little-unsigned-64, rest::binary>>, state) do
    state = compress(state, block, 2)
    process_blocks(rest, state)
  end

  defp process_blocks(remaining, state) when byte_size(remaining) < 8 do
    {state, remaining}
  end

  # Compress function - performs c rounds
  defp compress({v0, v1, v2, v3}, m, rounds) do
    {v0, v1, v2, v3} = {bxor(v0, m), v1, v2, v3}
    state = sipround({v0, v1, v2, v3}, rounds)
    {v0, v1, v2, v3} = state
    {bxor(v0, m), v1, v2, v3}
  end

  # Perform n SipRounds
  defp sipround(state, 0), do: state

  defp sipround({v0, v1, v2, v3}, n) do
    # Round operations
    v0 = add64(v0, v1)
    v1 = rotl64(v1, 13)
    v1 = bxor(v1, v0)
    v0 = rotl64(v0, 32)

    v2 = add64(v2, v3)
    v3 = rotl64(v3, 16)
    v3 = bxor(v3, v2)

    v0 = add64(v0, v3)
    v3 = rotl64(v3, 21)
    v3 = bxor(v3, v0)

    v2 = add64(v2, v1)
    v1 = rotl64(v1, 17)
    v1 = bxor(v1, v2)
    v2 = rotl64(v2, 32)

    sipround({v0, v1, v2, v3}, n - 1)
  end

  # Finalization
  defp finalize({v0, v1, v2, v3}, remaining, total_len) do
    # Build the last block with length encoding
    last_block = build_last_block(remaining, total_len)

    # Compress with the last block
    {v0, v1, v2, v3} = compress({v0, v1, v2, v3}, last_block, 2)

    # Finalization constant
    v2 = bxor(v2, 0xff)

    # 4 finalization rounds
    {v0, v1, v2, v3} = sipround({v0, v1, v2, v3}, 4)

    # Return hash
    bxor(bxor(bxor(v0, v1), v2), v3)
  end

  # Build the last block with remaining bytes and length encoding
  defp build_last_block(remaining, total_len) do
    # Pad remaining bytes to 8 bytes with length in last byte
    remaining_size = byte_size(remaining)
    padding_size = 7 - remaining_size

    padded =
      if padding_size > 0 do
        <<remaining::binary, 0::size(padding_size)-unit(8)>>
      else
        remaining
      end

    # Construct last block: remaining bytes + padding + length byte
    <<last_block::little-unsigned-56>> = padded
    len_byte = rem(total_len, 256)
    last_block ||| (len_byte <<< 56)
  end

  # 64-bit addition with wraparound
  @compile {:inline, add64: 2}
  defp add64(a, b) do
    band(a + b, 0xFFFFFFFFFFFFFFFF)
  end

  # 64-bit left rotation
  @compile {:inline, rotl64: 2}
  defp rotl64(value, shift) do
    value = band(value, 0xFFFFFFFFFFFFFFFF)
    band((value <<< shift) ||| (value >>> (64 - shift)), 0xFFFFFFFFFFFFFFFF)
  end
end
