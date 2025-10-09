# UUIDv47 - Privacy-Preserving UUID Facades

UUIDv47 is a novel approach to UUID generation that provides **time-ordered storage** with **privacy-preserving facades** for external APIs.

## Overview

The UUIDv47 scheme allows you to:

- **Store** time-ordered UUIDv7 in your database for excellent index performance
- **Emit** UUIDv4-looking facades at API boundaries for privacy
- **Convert** between the two formats deterministically and reversibly

This provides the best of both worlds: the performance benefits of time-ordered UUIDs internally, while presenting random-looking UUIDs externally.

## How It Works

UUIDv47 uses SipHash-2-4 cryptographic hashing to create a reversible mask:

1. Generate a standard UUIDv7 (time-ordered)
2. Use SipHash to generate a mask from the UUID's random bits and a secret key
3. XOR the timestamp with the mask and change version to 4
4. The result looks like a random UUIDv4 but can be unmasked back to the original

### Key Properties

- **Deterministic**: Same UUID + key always produces the same facade
- **Reversible**: Can always unmask back to the original with the correct key
- **Secure**: Without the key, the facade is indistinguishable from random UUIDv4
- **Fast**: Negligible performance overhead

## Installation

The UUIDv47 implementation is included in the `uniq` library. Simply use:

```elixir
alias Uniq.{UUID, UUIDv47}
```

## Basic Usage

### Generate and Mask

```elixir
# Generate a secret key (do this once and store securely)
key = UUIDv47.generate_key()
# => {0x0123456789abcdef, 0xfedcba9876543210}

# Generate a UUIDv7
uuid_v7 = UUID.uuid7(:raw)

# Mask it as a UUIDv4 facade
facade = UUIDv47.mask(uuid_v7, key)

# The facade looks like a random UUIDv4
UUID.to_string(facade)
# => "a3f2e1d4-c5b6-4a7b-8c9d-0e1f2a3b4c5d"
```

### Unmask

```elixir
# Unmask back to the original UUIDv7
original = UUIDv47.unmask(facade, key)

# Verify it's the same
original == uuid_v7
# => true
```

### Generate Facade Directly

```elixir
# Generate a new UUIDv7 and mask it in one step
facade = UUIDv47.generate(key)
# => "b4e3f2d1-c5a6-4b7a-8c9d-0e1f2a3b4c5d"
```

## Use Cases

### API Layer with Database Storage

```elixir
defmodule MyApp.Users do
  alias Uniq.{UUID, UUIDv47}

  # Store the key securely (e.g., in application config or secrets manager)
  @facade_key Application.compile_env!(:my_app, :uuid_facade_key)

  def create_user(attrs) do
    # Generate UUIDv7 for database
    user_id = UUID.uuid7(:raw)

    # Store in database with time-ordered benefits
    user = %User{id: user_id, ...}
    Repo.insert(user)

    # Return facade to API client
    %{user | id: UUIDv47.mask(user_id, @facade_key)}
  end

  def get_user(facade_id) do
    # Unmask the facade to get the real ID
    real_id = UUIDv47.unmask(facade_id, @facade_key)

    # Look up in database
    user = Repo.get(User, real_id)

    # Return with masked ID
    %{user | id: facade_id}
  end
end
```

### Multi-Tenant Systems

```elixir
defmodule MyApp.Tenants do
  # Different tenants can have different keys for additional isolation
  def tenant_key(tenant_id) do
    # Derive tenant-specific key from master key
    :crypto.hash(:sha256, "#{@master_key}-#{tenant_id}")
    |> binary_to_key()
  end

  def create_resource(tenant_id, attrs) do
    key = tenant_key(tenant_id)
    uuid_v7 = UUID.uuid7(:raw)

    # Store v7 in database
    resource = Repo.insert(%Resource{id: uuid_v7, ...})

    # Return tenant-specific facade
    %{resource | id: UUIDv47.mask(uuid_v7, key)}
  end
end
```

## Key Management

### Generating Keys

```elixir
# Generate a new random key
key = UUIDv47.generate_key()
# => {0x1234567890abcdef, 0xfedcba0987654321}

# Store securely (e.g., in environment variables, secrets manager, etc.)
```

### Key Storage

**DO NOT** hardcode keys in your source code. Use environment variables or a secrets management system:

```elixir
# config/runtime.exs
config :my_app, :uuid_facade_key, {
  System.fetch_env!("UUID_KEY_0") |> String.to_integer(16),
  System.fetch_env!("UUID_KEY_1") |> String.to_integer(16)
}
```

### Key Rotation

To rotate keys while maintaining backward compatibility:

1. Store a key version alongside UUIDs
2. Keep old keys for unmasking
3. Use new keys for masking

```elixir
defmodule MyApp.UUIDKeys do
  @current_key_version 2

  def current_key, do: get_key(@current_key_version)

  def get_key(1), do: {0x1234..., 0xfedcba...}  # Old key
  def get_key(2), do: {0xabcd..., 0x123456...}  # New key

  def mask(uuid), do: UUIDv47.mask(uuid, current_key())

  def unmask(facade, key_version) do
    key = get_key(key_version)
    UUIDv47.unmask(facade, key)
  end
end
```

## Performance

UUIDv47 operations are extremely fast:

- **Masking**: ~33 nanoseconds per operation
- **Unmasking**: ~33 nanoseconds per operation
- **Memory**: Zero allocations, pure computation

The overhead is negligible compared to typical database operations.

## Security Considerations

### Privacy Guarantees

- Without the key, facades are indistinguishable from random UUIDv4s
- Attackers cannot correlate facades across different keys
- Timestamp information is fully obfuscated

### Collision Probability

The probability of accidental collisions is extremely low:
- 1% chance at ~20 billion IDs
- 10% chance at ~60 billion IDs

### Attack Resistance

- Would require 2^74 IDs to have reasonable chance of key recovery
- SipHash-2-4 is cryptographically secure for this use case
- Random bits are preserved, maintaining UUID uniqueness

## Comparison with Alternatives

| Approach | Time-Ordered Storage | Privacy | Complexity |
|----------|---------------------|---------|------------|
| UUIDv4 only | ❌ Poor index performance | ✅ Random | ✅ Simple |
| UUIDv7 only | ✅ Excellent performance | ❌ Exposes timestamp | ✅ Simple |
| **UUIDv47** | ✅ Excellent performance | ✅ Privacy-preserving | ⚠️ Requires key management |
| Separate internal/external IDs | ✅ Can optimize | ✅ Full control | ❌ Complex, requires mapping table |

## Implementation Details

### SipHash-2-4

The implementation uses SipHash-2-4, a fast cryptographic hash function:

- **2** compression rounds per message block
- **4** finalization rounds
- 128-bit key (two 64-bit values)
- Produces 64-bit output

### Message Construction

The SipHash message is derived from the UUID's random bits:

```
byte 0:  0000 + (rand_a high 4 bits)
byte 1:  rand_a low 8 bits
byte 2:  00 + (rand_b high 6 bits)
bytes 3-9: rand_b low 56 bits
```

Total: 10 bytes (80 bits)

### Masking Algorithm

```elixir
# 1. Extract components from UUIDv7
<<timestamp::48, 7::4, rand_a::12, 2::2, rand_b::62>> = uuid_v7

# 2. Build SipHash message from random bits
message = build_message(rand_a, rand_b)

# 3. Generate mask
hash = SipHash.hash(key, message)
<<mask::48, _::16>> = <<hash::64>>

# 4. XOR timestamp and change version
masked_ts = timestamp XOR mask
<<masked_ts::48, 4::4, rand_a::12, 2::2, rand_b::62>>
```

## Helper Functions

```elixir
# Check if a UUID is a v7 (can be masked)
UUIDv47.v7?(uuid)
# => true/false

# Check if a UUID is a v4 facade (can be unmasked)
UUIDv47.facade?(uuid)
# => true/false

# Note: facade?/1 returns true for ANY valid v4 UUID,
# since facades are indistinguishable from regular v4 UUIDs
```

## Testing

Comprehensive tests are included:

```bash
# Run UUIDv47 tests
mix test test/uuid_v47_test.exs test/siphash_test.exs

# Run all tests
mix test
```

## References

- [UUIDv47 Specification](https://github.com/stateless-me/uuidv47)
- [RFC 4122 - UUID Standard](https://www.rfc-editor.org/rfc/rfc4122)
- [SipHash Paper](https://131002.net/siphash/siphash.pdf)
- [UUIDv7 Draft Spec](https://datatracker.ietf.org/doc/html/draft-peabody-dispatch-new-uuid-format)

## License

This implementation is part of the `uniq` library and follows its license.

## Contributing

Contributions are welcome! Please ensure:

1. All tests pass (`mix test`)
2. Code follows existing style
3. New features include tests and documentation
