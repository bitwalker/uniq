# Migrating From elixir_uuid

Migration from `elixir_uuid` is very simple, and you have 2 paths depending on how it is used
in your project today.

1. You no longer depend on `elixir_uuid` directly or indirectly
3. You no longer depend on `elixir_uuid` directly, but it is still present in your dependency tree

In the first scenario, all you need to do is replace any uses of `UUID` in your project with `Uniq.UUID`,
or simply alias `Uniq.UUID` in those modules.

NOTE: The `info/1` and `info!/1` functions return a struct by default, so if you use those functions and
you aren't planning to add the compatibility shim, you'll want to update those uses. See the function docs
for information on the structure.

In the second scenario - which also applies if you just want to add the dependency without making any code
changes - you must add an override dependency for `:elixir_uuid`, like so:

```elixir
defp deps do
  [
    {:uniq, "~> x.x"},
    {:elixir_uuid, "~> 0.1", hex: :uniq_compat, override: true},
  ]
end
```

This replaces the `:elixir_uuid` dependency with a shim that delegates to `:uniq` internally. With this
in place, `elixir_uuid` is removed from your dependency tree entirely.
