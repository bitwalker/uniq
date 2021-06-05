# Uniq

Uniq provides generation, formatting, parsing, and analysis of RFC 4122 UUIDs, with
support for the draft UUIDv6 extension. It is a package for Elixir projects, and can
be found on Hex as `:uniq`.

## Features

* Follows the RFC 4122 specification, i.e. supports UUID versions 1, 3, 4, and 5 as described in the RFC
* Supports UUIDv6, which is described in a proposed extension for RFC 4122, and improves upon desirable traits 
of both UUIDv1 and UUIDv4 to provide the best of both, while removing their downsides.  See 
[here](https://datatracker.ietf.org/doc/html/draft-peabody-dispatch-new-uuid-format) for more information on how it does so.
* Supports usage as an Ecto type, just use `Uniq.UUID` as the type of a field, and `:binary_id` in your migrations.
* Can be used as a drop-in replacement for `elixir_uuid`, just replace `elixir_uuid` with `uniq` in your dependencies, and you are good to go!

## Usage

The primary API is provided by the `Uniq.UUID` module.

To generate UUIDs, pick the version you want, and call the appropriate generator. For example:

* `uuid1/0`, generates UUIDv1 and formats it as a human-readable string, i.e. `6ba7b810-9dad-11d1-80b4-00c04fd430c8`
* `uuid1/1`, generates UUIDv1 in the specified format
* `uuid3/2`, generates UUIDv3 using the provided namespace and name, and formats it as a human-readable string
* `uuid3/3`, generates UUIDv3 using the provided namespace and name, in the specified format

See the [docs](https://hexdocs.pm/uniq) for the full set of functions available.

You can also convert UUID strings to/from the human-readable and binary formats; parse UUID strings/binaries; and determine their validity.

If you are replacing `elixir_uuid` with this library, you can continue to use the `UUID` module, as `uniq` defines it when not present, and exposes
the same interface that `elixir_uuid` does.

### Ecto

You can use the type provided by this library in lieu of `Ecto.UUID`, simply use `Uniq.UUID` 
where you would use `Ecto.UUID`, and specify `:binary_id` as the type of the column in your migrations.

## Installation

```elixir
def deps do
  [
    {:uniq, "~> 0.1"}
  ]
end
```

## License

Apache 2.0. See [LICENSE.md](LICENSE.md) for the full text.
