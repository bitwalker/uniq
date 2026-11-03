# Changelog

## Unreleased

### Added

- UUIDv47 facade generation and reversible masking, based on
  [PR #28](https://github.com/bitwalker/uniq/pull/28) by @epinault. Corrected
  SipHash message injection and low-48-bit mask selection for upstream
  interoperability; added official primitive vectors and independent C facade
  vectors. This is timestamp obfuscation, not authentication or a standardized
  UUID version. See the facade guide for key management and security boundaries.

- Opaque UUIDv8 parsing in every supported format, based on
  [PR #19](https://github.com/bitwalker/uniq/pull/19) by Vladimir Drobyshevskiy.
  The parser preserves payload bytes without inventing timestamp, sequence, or
  node metadata. Existing v8 values can be stored through Ecto; v8 is not an
  autogeneration option.

### Changed

- `valid?(uuid, strict: true)` consistently restricts UUID versions to the legacy
  set (1, 3, 4, 5, plus the nil UUID). UUIDv7, previously accepted by this option,
  and UUIDv8 require default validation. Remove `strict: true` when validating
  RFC 9562 v6/v7/v8 values. Historical variant handling for v1–v5 is unchanged.

- The optional Ecto integration requires Ecto `~> 3.12`, matching the
  parameterized-type representation used by ScopedUUID.

### Fixed

- UUIDv1 and UUIDv6 now encode 100-nanosecond ticks since 1582-10-15, as required
  by RFC 9562. The previous calculation used the wrong epoch and scale and lost
  fractional-second information.

### Migrating UUIDv1/v6 generation

Newly generated v1/v6 timestamps jump **forward** from the old range: around
November 2026, from approximately `6.4 × 10^15` to `1.4 × 10^17`. At present-day
dates, the old and corrected generators occupy separate timestamp ranges; the
transition does not introduce overlap between those ranges.

Existing UUIDs keep their bytes, remain readable, and require no primary-key or
foreign-key migration. Their incorrectly encoded historical timestamps are not
rewritten, and the lost fractional information cannot be recovered from the ID.

Applications using UUID timestamps for time-range queries, chronological
pagination, or ordering should account for the discontinuity. During a rolling
upgrade, an old worker can produce a lower-timestamp ID after an upgraded worker
has produced a higher-timestamp ID. Coordinate the rollout if this ordering
matters, or use an explicit creation-time column. Neither generator promises
strict monotonicity during wall-clock rollback. UUIDv7 generation is unchanged.
