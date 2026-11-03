defmodule Uniq.UUIDv47 do
  @moduledoc """
  Reversible UUIDv7-to-v4 facades using the UUIDv47 scheme.

  Store a UUIDv7 internally and mask its timestamp when presenting a v4-shaped
  identifier externally. This is a facade transformation, not a standardized
  UUID version 47. The timestamp is XORed with the low 48 bits of SipHash-2-4
  over the UUID's 74 random bits. Those random bits are preserved.

  Masking is deterministic and does not authenticate identifiers. Ordinary v4
  values pass `facade?/1`, and any valid key decodes them to v7-shaped values.
  Wrong keys and tampering are not detected. Preserved random bits allow
  correlation with the original UUID and across key changes.

  Keep keys secret and stable for the lifetime of issued facades. Store key IDs
  outside the UUID when rotating keys. Use normal authorization checks for
  decoded identifiers. See the [UUIDv47 guide](guides/UUID_V47.md) and the
  [upstream scheme](https://github.com/stateless-me/uuidv47).
  """

  import Bitwise, only: [band: 2, bxor: 2]
  alias Uniq.{SipHash, UUID}

  @type key :: SipHash.key()
  @type uuid :: <<_::128>>
  @formats [:default, :hex, :urn, :slug, :raw]

  @doc """
  Generates two unsigned 64-bit key words from a cryptographically secure source.

      iex> {k0, k1} = Uniq.UUIDv47.generate_key()
      iex> k0 in 0..0xFFFFFFFFFFFFFFFF and k1 in 0..0xFFFFFFFFFFFFFFFF
      true
  """
  @spec generate_key() :: key
  def generate_key do
    <<k0::unsigned-64, k1::unsigned-64>> = :crypto.strong_rand_bytes(16)
    {k0, k1}
  end

  @doc """
  Masks an RFC-variant UUIDv7 raw binary as a v4-shaped raw binary.

  The key is a tuple of numeric unsigned 64-bit words `{k0, k1}`, matching the
  upstream C API. A standard SipHash byte key maps to little-endian words.

      iex> uuid = Uniq.UUID.string_to_binary!("00000000-0000-7000-8000-000000000000")
      iex> facade = Uniq.UUIDv47.mask(uuid, {0x0123456789ABCDEF, 0xFEDCBA9876543210})
      iex> Uniq.UUID.to_string(facade)
      "22d97126-9609-4000-8000-000000000000"
  """
  @spec mask(uuid, key) :: uuid
  def mask(<<timestamp::48, 7::4, rand_a::12, 2::2, rand_b::62>>, key),
    do: transform(timestamp, rand_a, rand_b, key, 4)

  def mask(_uuid, _key),
    do: raise(ArgumentError, "Expected a UUIDv7 raw binary with the RFC variant")

  @doc """
  Unmasks an RFC-variant v4 raw binary using the original key.

  This checks only the input's shape. Any v4 value and valid key produce a
  v7-shaped result; success does not establish provenance or validate the key.

      iex> uuid = Uniq.UUID.uuid7(:raw)
      iex> key = {0x0123456789ABCDEF, 0xFEDCBA9876543210}
      iex> Uniq.UUIDv47.unmask(Uniq.UUIDv47.mask(uuid, key), key) == uuid
      true
  """
  @spec unmask(uuid, key) :: uuid
  def unmask(<<timestamp::48, 4::4, rand_a::12, 2::2, rand_b::62>>, key),
    do: transform(timestamp, rand_a, rand_b, key, 7)

  def unmask(_uuid, _key),
    do: raise(ArgumentError, "Expected a UUIDv4 facade raw binary with the RFC variant")

  defp transform(timestamp, rand_a, rand_b, key, version) do
    message = <<0::4, rand_a::12, 0::2, rand_b::62>>
    mask = band(SipHash.hash(key, message), 0xFFFFFFFFFFFF)
    <<bxor(timestamp, mask)::48, version::4, rand_a::12, 2::2, rand_b::62>>
  end

  @doc """
  Generates a UUIDv7 and masks it, returning the requested UUID format.

      iex> facade = Uniq.UUIDv47.generate(Uniq.UUIDv47.generate_key())
      iex> byte_size(facade)
      36
  """
  @spec generate(key, UUID.format()) :: UUID.formatted()
  def generate(key, format \\ :default)

  def generate(key, format) when format in @formats,
    do: UUID.uuid7(:raw) |> mask(key) |> UUID.format(format)

  def generate(_key, _format), do: raise(ArgumentError, "expected a supported UUID format")

  @doc "Checks raw RFC-variant v4 shape, including ordinary v4 UUIDs; does not authenticate."
  @spec facade?(term) :: boolean
  def facade?(<<_::48, 4::4, _::12, 2::2, _::62>>), do: true
  def facade?(_), do: false

  @doc "Checks whether a raw binary is an RFC-variant UUIDv7 that can be masked."
  @spec v7?(term) :: boolean
  def v7?(<<_::48, 7::4, _::12, 2::2, _::62>>), do: true
  def v7?(_), do: false
end
