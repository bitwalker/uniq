# Changelog

## Unreleased

### Changed

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
