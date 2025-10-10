defmodule Uniq.UUIDv47 do
  @moduledoc """
  UUIDv47 implementation for privacy-preserving UUID facades.

  UUIDv47 allows you to:
  - Store time-ordered UUIDv7 in your database
  - Emit UUIDv4-looking facades at API boundaries
  - Maintain cryptographic privacy with reversible masking

  The masking uses SipHash-2-4 to XOR the timestamp with a keyed hash
  derived from the UUID's own random bits, making the transformation
  deterministic and fully invertible.

  ## Key Management

  Keys should be 128 bits (two 64-bit integers). For production use:
  - Generate keys using a cryptographically secure random source
  - Consider using HKDF for key derivation
  - Implement key rotation by storing a small key ID with UUIDs

  ## Examples

      # Generate a UUIDv7
      uuid_v7 = Uniq.UUID.uuid7(:raw)

      # Generate a 128-bit key
      key = Uniq.UUIDv47.generate_key()

      # Mask it as a v4 facade
      uuid_v4_facade = Uniq.UUIDv47.mask(uuid_v7, key)

      # Unmask back to v7
      ^uuid_v7 = Uniq.UUIDv47.unmask(uuid_v4_facade, key)

  ## References

  * [UUIDv47 Specification](https://github.com/stateless-me/uuidv47)
  * [RFC 4122 - UUIDs](https://www.rfc-editor.org/rfc/rfc4122)
  """

  import Bitwise
  alias Uniq.SipHash

  @type key :: {non_neg_integer, non_neg_integer}
  @type uuid :: <<_::128>>

  @doc """
  Generates a cryptographically secure 128-bit key for UUIDv47 operations.

  Returns a tuple of two 64-bit integers `{k0, k1}`.

  ## Examples

      iex> {k0, k1} = Uniq.UUIDv47.generate_key()
      iex> is_integer(k0) and is_integer(k1)
      true

  """
  @spec generate_key() :: key
  def generate_key do
    <<k0::unsigned-64, k1::unsigned-64>> = :crypto.strong_rand_bytes(16)
    {k0, k1}
  end

  @doc """
  Masks a UUIDv7 as a UUIDv4 facade.

  Takes a UUIDv7 (as a 16-byte binary) and a 128-bit key, and returns
  a UUIDv4-looking facade. The facade appears random but can be
  deterministically unmasked back to the original UUIDv7.

  ## Parameters

  - `uuid_v7`: A 16-byte binary representing a UUIDv7
  - `key`: A tuple `{k0, k1}` of two 64-bit integers

  ## Returns

  A 16-byte binary representing the masked UUID with version 4.

  ## Examples

      iex> uuid_v7 = Uniq.UUID.uuid7(:raw)
      iex> key = {0x0123456789abcdef, 0xfedcba9876543210}
      iex> facade = Uniq.UUIDv47.mask(uuid_v7, key)
      iex> <<_::48, version::4, _::76>> = facade
      iex> version
      4

  """
  @spec mask(uuid, key) :: uuid
  def mask(
        <<timestamp::big-unsigned-48, 7::4, rand_a::12, variant::2, rand_b::62>> = _uuid_v7,
        key
      ) do
    # Build the message for SipHash from the random bits (must be 10 bytes)
    # byte 0: upper 4 bits = 0, lower 4 bits = rand_a high 4 bits
    # byte 1: rand_a low 8 bits
    # byte 2: upper 2 bits = 0, lower 6 bits = rand_b high 6 bits
    # bytes 3-9: rand_b low 56 bits

    <<rand_a_hi::4, rand_a_lo::8>> = <<rand_a::12>>
    <<rand_b_hi::6, rand_b_rest::56>> = <<rand_b::62>>

    message = <<0::4, rand_a_hi::4, rand_a_lo::8, 0::2, rand_b_hi::6, rand_b_rest::56>>

    # Compute SipHash and extract 48 bits for masking
    hash = SipHash.hash(key, message)
    <<mask_value::48, _::16>> = <<hash::unsigned-64>>

    # XOR timestamp with mask
    masked_timestamp = bxor(timestamp, mask_value)

    # Return v4 facade
    <<masked_timestamp::big-unsigned-48, 4::4, rand_a::12, variant::2, rand_b::62>>
  end

  def mask(uuid, _key) do
    raise ArgumentError, """
    Expected a UUIDv7 (version 7), but got a different UUID version.

    UUID bytes: #{inspect(uuid, base: :hex)}

    To use UUIDv47, you must start with a UUIDv7:

        uuid_v7 = Uniq.UUID.uuid7(:raw)
        facade = Uniq.UUIDv47.mask(uuid_v7, key)
    """
  end

  @doc """
  Unmasks a UUIDv4 facade back to the original UUIDv7.

  Takes a masked UUID facade and the same key used for masking,
  and recovers the original UUIDv7.

  ## Parameters

  - `facade`: A 16-byte binary representing the masked UUID (version 4)
  - `key`: The same tuple `{k0, k1}` used for masking

  ## Returns

  A 16-byte binary representing the original UUIDv7.

  ## Examples

      iex> uuid_v7 = Uniq.UUID.uuid7(:raw)
      iex> key = {0x0123456789abcdef, 0xfedcba9876543210}
      iex> facade = Uniq.UUIDv47.mask(uuid_v7, key)
      iex> Uniq.UUIDv47.unmask(facade, key) == uuid_v7
      true

  """
  @spec unmask(uuid, key) :: uuid
  def unmask(
        <<masked_timestamp::big-unsigned-48, 4::4, rand_a::12, variant::2, rand_b::62>> = _facade,
        key
      ) do
    # Build the same message from random bits (must be 10 bytes)
    <<rand_a_hi::4, rand_a_lo::8>> = <<rand_a::12>>
    <<rand_b_hi::6, rand_b_rest::56>> = <<rand_b::62>>

    message = <<0::4, rand_a_hi::4, rand_a_lo::8, 0::2, rand_b_hi::6, rand_b_rest::56>>

    # Compute the same mask
    hash = SipHash.hash(key, message)
    <<mask_value::48, _::16>> = <<hash::unsigned-64>>

    # XOR to recover original timestamp
    timestamp = bxor(masked_timestamp, mask_value)

    # Return v7 UUID
    <<timestamp::big-unsigned-48, 7::4, rand_a::12, variant::2, rand_b::62>>
  end

  def unmask(uuid, _key) do
    raise ArgumentError, """
    Expected a UUIDv4 facade, but got a different UUID version.

    UUID bytes: #{inspect(uuid, base: :hex)}

    To unmask a facade, it must have been created with mask/2:

        facade = Uniq.UUIDv47.mask(uuid_v7, key)
        original = Uniq.UUIDv47.unmask(facade, key)
    """
  end

  @doc """
  Generates a new UUIDv7 and immediately masks it as a v4 facade.

  This is a convenience function that combines `Uniq.UUID.uuid7/1`
  and `mask/2`.

  ## Parameters

  - `key`: A tuple `{k0, k1}` of two 64-bit integers
  - `format`: The output format (default: `:default`)

  ## Returns

  A masked UUID in the specified format.

  ## Examples

      iex> key = Uniq.UUIDv47.generate_key()
      iex> facade = Uniq.UUIDv47.generate(key)
      iex> String.length(facade)
      36

  """
  @spec generate(key, Uniq.UUID.format()) :: Uniq.UUID.formatted()
  def generate(key, format \\ :default) do
    Uniq.UUID.uuid7(:raw)
    |> mask(key)
    |> Uniq.UUID.format(format)
  end

  @doc """
  Verifies that a UUID is a valid v4 facade that can be unmasked.

  This checks that the UUID has version 4 and the correct variant bits.

  ## Examples

      iex> uuid_v7 = Uniq.UUID.uuid7(:raw)
      iex> key = Uniq.UUIDv47.generate_key()
      iex> facade = Uniq.UUIDv47.mask(uuid_v7, key)
      iex> Uniq.UUIDv47.facade?(facade)
      true

  """
  @spec facade?(uuid) :: boolean
  def facade?(<<_::48, 4::4, _::12, 2::2, _::62>>), do: true
  def facade?(_), do: false

  @doc """
  Verifies that a UUID is a valid UUIDv7 that can be masked.

  This checks that the UUID has version 7 and the correct variant bits.

  ## Examples

      iex> uuid_v7 = Uniq.UUID.uuid7(:raw)
      iex> Uniq.UUIDv47.v7?(uuid_v7)
      true

  """
  @spec v7?(uuid) :: boolean
  def v7?(<<_::48, 7::4, _::12, 2::2, _::62>>), do: true
  def v7?(_), do: false
end
