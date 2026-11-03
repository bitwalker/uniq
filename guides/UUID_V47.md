# UUIDv47 facades

`Uniq.UUIDv47` implements the [UUIDv47 facade scheme](https://github.com/stateless-me/uuidv47):
store an ordinary UUIDv7 internally and expose a reversible, UUIDv4-shaped value.
The name describes a v7-to-v4 transformation; it is not a standardized UUID version.

The implementation is based on [PR #28](https://github.com/bitwalker/uniq/pull/28)
by @epinault, with corrections checked against the upstream C implementation
and the official SipHash-2-4 known-answer vectors.

## Usage

```elixir
# Generate once and retain securely; do not generate a new key for each request.
key = Uniq.UUIDv47.generate_key()
uuid = Uniq.UUID.uuid7(:raw)
facade = Uniq.UUIDv47.mask(uuid, key)
external_id = Uniq.UUID.to_string(facade)

# Convert formatted input to raw bytes before unmasking.
original = external_id |> Uniq.UUID.string_to_binary!() |> Uniq.UUIDv47.unmask(key)
true = original == uuid

# Or generate a facade directly in any normal Uniq format:
slug = Uniq.UUIDv47.generate(key, :slug)
```

`mask/2` accepts a 16-byte RFC-variant UUIDv7 and returns a 16-byte v4 facade.
`unmask/2` accepts the corresponding RFC-variant v4 shape. Unsupported versions,
variants, malformed inputs, and invalid keys raise `ArgumentError`.
`generate/2` accepts `:default`, `:hex`, `:urn`, `:slug`, and `:raw` formats.

## Keys and interoperability

Keys are tuples `{k0, k1}` of unsigned 64-bit numeric words, matching the upstream
C API. When importing a standard 16-byte SipHash key, decode each eight-byte word
as little endian:

```elixir
<<k0::little-unsigned-64, k1::little-unsigned-64>> = key_bytes
key = {k0, k1}
```

The timestamp mask is the low 48 bits of SipHash-2-4 over the ten-byte message
`<<0::4, rand_a::12, 0::2, rand_b::62>>`. For example, key words
`{0x0123456789ABCDEF, 0xFEDCBA9876543210}` map
`00000000-0000-7000-8000-000000000000` to
`22d97126-9609-4000-8000-000000000000`.

Store keys outside source control and retain them while their facades remain in
use. Rotation needs an external key identifier or another application-managed
way to select the original key: the UUID contains no key identifier, and a wrong
key cannot be detected by decoding. Changing keys changes public identifiers.

## Security boundaries

The transformation obscures the timestamp using a keyed hash and preserves all
74 random bits. It is deterministic and, for a fixed key, reversible without
introducing collisions between distinct valid source UUIDs.

Preserved random bits allow correlation with the original UUID and across key
changes. Repeated random fields reuse the timestamp mask; XORing two masked
timestamps then reveals the XOR of their original timestamps. The scheme does
not provide general encryption, authentication, or unlinkability guarantees.

`facade?/1` checks only v4 shape and variant: ordinary v4 UUIDs pass. Every such
value decodes to a v7-shaped value under every valid key. Successful decoding
cannot establish origin, detect tampering, or validate which key was used.
Perform normal database lookups and authorization checks on decoded identifiers;
a facade is not an access token.
