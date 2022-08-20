defmodule Uniq.Test.Generators do
  import ExUnitProperties
  import StreamData

  alias Uniq.UUID

  @reserved_ncs <<0::1>>
  @rfc_variant <<2::2>>
  @reserved_ms <<6::3>>
  @reserved_future <<7::3>>
  @rfc_versions [1, 3, 4, 5]
  @versions [1, 3, 4, 5, 6, 7]
  @variants [@reserved_ncs, @rfc_variant, @reserved_ms, @reserved_future]
  @reserved_variants [@reserved_ncs, @reserved_ms, @reserved_future]
  @reserved_variants_uniform [<<0::3>>, <<6::3>>, <<7::3>>]

  @formats [:raw, :default, :hex, :urn, :slug]

  def valid_uuid(format \\ :raw) when format in @formats do
    gen all(
          {version, variant} <-
            bind(member_of(@variants), fn variant ->
              # Version 6 requires the use of the correct variant to be valid
              if variant in @reserved_variants do
                bind(member_of(@rfc_versions), fn version -> constant({version, variant}) end)
              else
                bind(member_of(@versions), fn version -> constant({version, variant}) end)
              end
            end),
          bits <- bitstring(length: 128)
        ) do
      variant_size = bit_size(variant)
      rest_size = 64 - variant_size
      <<start::48, _::4, mid::12, _::size(variant_size), rest::size(rest_size)>> = bits

      uuid =
        <<start::48, version::4, mid::12, variant::bitstring-size(variant_size),
          rest::size(rest_size)>>
        |> UUID.format(format)

      {version, variant, uuid}
    end
  end

  def invalid_uuid(format \\ :raw) when format in @formats do
    gen all(
          bits <-
            bind(bitstring(length: 128), fn <<start::48, v::4, mid::12, var::bitstring-size(3),
                                              rest::61>> = bits ->
              case v do
                v when v in [6, 7] ->
                  # Version 6 specifically only allows a single variant to be considered valid
                  case var do
                    <<@rfc_variant, _::1>> ->
                      bind(member_of(@reserved_variants_uniform), fn variant ->
                        constant(
                          <<start::48, v::4, mid::12, variant::bitstring-size(3), rest::61>>
                        )
                      end)

                    _ ->
                      constant(bits)
                  end

                v when v in @rfc_versions ->
                  # Any 3-bit pattern is technically valid as a variant in a UUID per the RFC, so we instead generate
                  # a known-invalid version.
                  bind(integer(8..15), fn version ->
                    constant(<<start::48, version::4, mid::12, var::bitstring-size(3), rest::61>>)
                  end)

                _ ->
                  constant(bits)
              end
            end)
        ) do
      <<_::48, version::4, _::12, variant::bitstring-size(3), _::61>> = bits
      {version, variant, UUID.format(bits, format)}
    end
  end
end
