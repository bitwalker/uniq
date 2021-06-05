# Migrating From elixir_uuid

If you are replacing `elixir_uuid` with `uniq`, there are two scenarios:

1. You no longer depend on `elixir_uuid` directly or indirectly
2. You no longer depend on `elixir_uuid` directly, but it is still present in your dependency tree

In the first scenario, `uniq` provides a shim `UUID` module that matches the API provided by `elixir_uuid`, so legacy code will continue to
work without requiring any immediate changes. You should prefer to switch to `Uniq.UUID` though, or alias `Uniq.UUID` as `UUID` so that it
is unambiguous which library is referred to.

In the second scenario, `uniq` cannot shim `UUID`, so you need to change any uses of `UUID` in your own code to `Uniq.UUID`,
or alias `Uniq.UUID` as `UUID` where used.
