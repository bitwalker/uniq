defmodule Uniq.UUID do
  @moduledoc """
  This module provides RFC 4122 compliant universally unique identifiers (UUIDs).

  Features:

  * Supports variants 1-5 as described in the RFC
  * Supports UUID v6, as proposed [here](https://datatracker.ietf.org/doc/html/draft-peabody-dispatch-new-uuid-format),
  which provides database-friendly lexicographical sorting by timestamp, as well as other improvements to the original RFC.
  * Supports formatting/parsing various string formats, including the raw binary encoding
  * Can be used in place of `Ecto.UUID`
  * Supports serialization to/from JSON, using the human-readable string format
  """
  use Bitwise, skip_operators: true

  import Kernel, except: [to_string: 1]

  defstruct [:format, :version, :variant, :time, :seq, :node, :bytes]

  @compile {:inline, [format_default: 1, c: 1]}

  @type t :: <<_::128>>
  @type format :: :default | :raw | :hex | :urn | :slug
  @type namespace :: :dns | :url | :oid | :x500 | nil

  @type info :: %__MODULE__{
          format: :raw | :hex | :default | :urn | :slug,
          version: 1..8,
          variant: bitstring,
          time: non_neg_integer,
          seq: non_neg_integer,
          node: <<_::48>>,
          bytes: t
        }

  @formats [:default, :raw, :hex, :urn, :slug]
  @namespaces [:dns, :url, :oid, :x500, nil]

  # Variants
  @reserved_ncs <<0::1>>
  @rfc_variant <<2::2>>
  @reserved_ms <<6::3>>
  @reserved_future <<7::3>>

  defmacrop bits(n), do: quote(do: bitstring - size(unquote(n)))
  defmacrop bytes(n), do: quote(do: binary - size(unquote(n)))
  defmacrop uint(n), do: quote(do: unsigned - integer - size(unquote(n)))
  defmacrop biguint(n), do: quote(do: big - unsigned - integer - size(unquote(n)))

  @doc """
  Generates a UUID using the version 1 scheme, as described in RFC 4122

  This scheme is based on a few key properties:

  * A timestamp, based on the count of 100-nanosecond intervals since the start of
  the Gregorian calendar, i.e. October 15th, 1582, in Coordinated Universal Time (UTC).
  * A clock sequence number, used to ensure that UUIDs generated with the same timestamp
  are still unique, by incrementing the sequence each time a UUID is generated with the
  same timestamp as the last UUID that was generated. This sequence is initialized with
  random bytes at startup, to protect against conflicts.
  * A node identifier, which is based on the MAC address of one of the network interfaces
  on the system, or if unavailable, using random bytes. In our case, we specifically look
  for the first network interface returned by `:inet.getifaddrs/0` that is up, broadcastable,
  and has a hardware address, otherwise falling back to cryptographically strong random bytes.
  """
  @spec uuid1() :: t
  @spec uuid1(format) :: t
  def uuid1(format \\ :default) do
    {time, clock} = Uniq.Generator.next()

    uuid1(time, clock, mac_address(), format)
  end

  @doc """
  This function is the same as `uuid/1`, except the caller provides the clock sequence
  value and the node identifier (which must be a 6-byte binary).

  See `uuid/1` for details.
  """
  @spec uuid1(clock_seq :: non_neg_integer, node :: <<_::48>>, format) :: t
  def uuid1(clock_seq, <<node::bits(48)>>, format \\ :default)
      when is_integer(clock_seq) and format in @formats do
    {time, _} = Uniq.Generator.next()

    uuid1(time, clock_seq, node, format)
  end

  defp uuid1(time, clock_seq, node, format) do
    <<thi::12, tmid::16, tlo::32>> = <<time::biguint(60)>>

    # Encode version into high bits of timestamp
    thi = bor(thi, bsl(1, 12))

    # Encode variant into high bits of clock sequence
    clock_hi = bsr(band(clock_seq, 0x3F00), 8)
    clock_hi = bor(clock_hi, 0x80)
    clock_lo = band(clock_seq, 0xFF)

    raw = <<tlo::32, tmid::16, thi::16, clock_hi::8, clock_lo::8, node::bits(48)>>

    format(raw, format)
  end

  @doc """
  Generates a UUID using the version 3 scheme, as described in RFC 4122

  This scheme provides the means for generating UUIDs deterministically,
  given a namespace and a name. This means that with the same inputs, you
  get the same UUID as output.

  The main difference between this and the version 5 scheme, is that version 3
  uses MD5 for hashing, and version 5 uses SHA1. Both hashes are deprecated these
  days, but you should prefer version 5 unless otherwise required.

  In this scheme, the timestamp, clock sequence and node value are constructed
  from the namespace and name, as described in RFC 4122, Section 4.3.

  ## Namespaces

  You may choose one of several options for namespacing your UUIDs:

  1. Use a predefined namespace. These are provided by RFC 4122 in order to provide
  namespacing for common types of names. See below.
  2. Use your own namespace. For this, simply generate a UUID to represent the namespace.
  You may provide this UUID in whatever format is supported by `parse/1`.
  3. Use `nil`. This is bound to a special-case UUID that has no intrinsic meaning, but is
  valid for use as a namespace.

  The set of predefined namespaces consist of the following:

  * `:dns`, intended for namespacing fully-qualified domain names
  * `:url`, intended for namespacing URLs
  * `:oid`, intended for namespacing ISO OIDs
  * `:x500`, intended for namespacing X.500 DNs (in DER or text output format)

  ## Notes

  One thing to be aware of with version 3 and 5 UUIDs, is that unlike version 1 and 6,
  the lexicographical ordering of UUIDs of generated one after the other, is entirely
  random, as the most significant bits are dependent upon the hash of the namespace and
  name, and thus not based on time or even the lexicographical ordering of the name.

  This is generally worth the tradeoff in favor of determinism, but it is something to
  be aware of.

  Likewise, since the generation is deterministic, care must be taken to ensure that you
  do not try to use the same name for two different objects within the same namespace. This
  should be obvious, but since the other schemes are _not_ sensitive in this way, it is worth
  calling out.
  """
  @spec uuid3(namespace, name :: binary) :: t
  @spec uuid3(namespace, name :: binary, format) :: t
  def uuid3(namespace, name, format \\ :default)
      when (namespace in @namespaces or is_binary(namespace)) and is_binary(name) and
             format in @formats do
    namespaced_uuid(3, :md5, namespace, name, format)
  end

  @doc """
  Generates a UUID using the version 4 scheme, as described in RFC 4122

  This scheme is like the version 1 scheme, except it uses randomly generated data
  for the timestamp, clock sequence, and node fields.

  This scheme is the closest you can get to truly unique identifiers, as they are based
  on truly random (or pseudo-random) data, so the chances of generating the same UUID
  twice is astronomically small.

  ## Notes

  The version 4 scheme does have some deficiencies. Namely, since they are based on random
  data, the lexicographical ordering of the resulting UUID is itself random, which can play havoc
  with database indices should you choose to use UUIDs for primary keys.

  It is strongly recommended to consider the version 6 scheme instead. They are almost the
  same as a version 1 UUID, but with improved semantics that combine some of the beneficial
  traits of version 4 UUIDs without the lexicographical ordering downsides. The only caveat
  to that recommendation is if you need to pass them through a system that inspects the UUID
  encoding itself and doesn't have preliminary support for version 6.
  """
  @spec uuid4() :: t
  @spec uuid4(format) :: t
  def uuid4(format \\ :default) when format in @formats do
    <<tlo_mid::48, _::4, thi::12, _::2, rest::62>> = :crypto.strong_rand_bytes(16)

    raw = <<tlo_mid::48, 4::biguint(4), thi::12, @rfc_variant, rest::62>>

    format(raw, format)
  end

  @doc """
  Generates a UUID using the version 5 scheme, as described in RFC 4122

  This scheme provides the means for generating UUIDs deterministically,
  given a namespace and a name. This means that with the same inputs, you
  get the same UUID as output.

  The main difference between this and the version 5 scheme, is that version 3
  uses MD5 for hashing, and version 5 uses SHA1. Both hashes are deprecated these
  days, but you should prefer version 5 unless otherwise required.

  In this scheme, the timestamp, clock sequence and node value are constructed
  from the namespace and name, as described in RFC 4122, Section 4.3.

  ## Namespaces

  You may choose one of several options for namespacing your UUIDs:

  1. Use a predefined namespace. These are provided by RFC 4122 in order to provide
  namespacing for common types of names. See below.
  2. Use your own namespace. For this, simply generate a UUID to represent the namespace.
  You may provide this UUID in whatever format is supported by `parse/1`.
  3. Use `nil`. This is bound to a special-case UUID that has no intrinsic meaning, but is
  valid for use as a namespace.

  The set of predefined namespaces consist of the following:

  * `:dns`, intended for namespacing fully-qualified domain names
  * `:url`, intended for namespacing URLs
  * `:oid`, intended for namespacing ISO OIDs
  * `:x500`, intended for namespacing X.500 DNs (in DER or text output format)

  ## Notes

  One thing to be aware of with version 3 and 5 UUIDs, is that unlike version 1 and 6,
  the lexicographical ordering of UUIDs of generated one after the other, is entirely
  random, as the most significant bits are dependent upon the hash of the namespace and
  name, and thus not based on time or even the lexicographical ordering of the name.

  This is generally worth the tradeoff in favor of determinism, but it is something to
  be aware of.

  Likewise, since the generation is deterministic, care must be taken to ensure that you
  do not try to use the same name for two different objects within the same namespace. This
  should be obvious, but since the other schemes are _not_ sensitive in this way, it is worth
  calling out.
  """
  @spec uuid5(namespace, name :: binary) :: t
  @spec uuid5(namespace, name :: binary, format) :: t
  def uuid5(namespace, name, format \\ :default)
      when (namespace in @namespaces or is_binary(namespace)) and is_binary(name) and
             format in @formats do
    namespaced_uuid(5, :sha, namespace, name, format)
  end

  @doc """
  Generates a UUID using the proposed version 6 scheme,
  found [here](https://datatracker.ietf.org/doc/html/draft-peabody-dispatch-new-uuid-format).
  This is a draft extension of RFC 4122, but has not yet been formally accepted.

  Version 6 provides the following benefits over versions 1 and 4:

  * Like version 1, it is time-based, but unlike version 1, it is naturally sortable by time
  in its raw binary encoded form
  * Like version 4, it provides better guarantees of uniqueness and privacy, by basing itself
  on random or pseudo-random data, rather than MAC addresses and other potentially sensitive
  information.
  * Unlike version 4, which tends to interact poorly with database indices due to being derived
  entirely from random or psuedo-random data; version 6 ensures that the most significant bits
  of the binary encoded form are a 1:1 match with the most significant bits of the timestamp on
  which it was derived. This guarantees that version 6 UUIDs are naturally sortable in the order
  in which they were generated (with some randomness among those which are generated at the same
  time).

  There have been a number of similar proposals that address the same set of flaws. For example:

  * [KSUID](https://github.com/segmentio/ksuid)
  * [ULID](https://github.com/ulid/spec)
  """
  @spec uuid6() :: t
  @spec uuid6(format) :: t
  def uuid6(format \\ :default) when format in @formats do
    {time, clock} = Uniq.Generator.next()
    node = :crypto.strong_rand_bytes(6)

    # Deconstruct timestamp
    <<thi::48, tlo::12>> = <<time::biguint(60)>>

    # Encode the version to the most significant bits of the last octet of the timestamp
    tlo_and_version = <<6::4, tlo::12>>

    # Encode the variant in the most significant bits of the clock sequence
    clock_seq = <<@rfc_variant, clock::biguint(14)>>

    raw = <<thi::48, tlo_and_version::bits(16), clock_seq::bits(16), node::bits(48)>>

    format(raw, format)
  end

  defp namespaced_uuid(version, algorithm, namespace, name, format) do
    id = namespace_id(namespace)

    <<tlo_mid::48, _::4, thi::12, _::2, rest::62>> = hash(algorithm, id <> name)

    raw = <<tlo_mid::48, version::4, thi::12, @rfc_variant, rest::62>>

    format(raw, format)
  end

  @doc """
  Like `info/1`, but raises if the input UUID is invalid.
  """
  @spec info!(binary, :struct) :: info | no_return
  @spec info!(binary, :keyword) :: Keyword.t() | no_return
  def info!(bin, style \\ :struct)

  def info!(bin, style) when is_binary(bin) do
    with {:ok, info} <- info(bin, style) do
      info
    else
      {:error, reason} ->
        raise ArgumentError, message: "invalid uuid: #{inspect(reason)}"
    end
  end

  def info!(_, _) do
    raise ArgumentError, message: "invalid uuid: :invalid_format"
  end

  @doc """
  This function parses the given UUID, in any of the supported encodings/formats, and produces
  the information gleaned from the encoded data.

  Two styles of information are supported, depending on whether the function is called via
  the compatibility shim for `:elixir_uuid`, or directly. You may pass `:struct` or `:keyword`
  manually if you wish to express a preference for one style or the other.

  The `:struct` form is the UUID structure used internally by this library, and it contains all
  of the information needed to re-encode the UUID as binary.

  The `:keyword` form matches 1:1 the keyword list produced by `UUID.info/1` provided by the
  `:elixir_uuid` library, and it contains slightly less information, but is useful for compatibility
  with legacy code that operates on that structure.

  # Examples

      iex> UUID.info("870df8e8-3107-4487-8316-81e089b8c2cf")
      {:ok, [uuid: "870df8e8-3107-4487-8316-81e089b8c2cf",
       binary: <<135, 13, 248, 232, 49, 7, 68, 135, 131, 22, 129, 224, 137, 184, 194, 207>>,
       type: :default,
       version: 4,
       variant: :rfc4122]}

      iex> Uniq.UUID.info("870df8e8-3107-4487-8316-81e089b8c2cf")
      {:ok, %Uniq.UUID{
       format: :default,
       version: 4,
       variant: <<2::2>>,
       time: 326283406408022248,
       seq: 790,
       node: <<129, 224, 137, 184, 194, 207>>,
       bytes: <<135, 13, 248, 232, 49, 7, 68, 135, 131, 22, 129, 224, 137, 184, 194, 207>>,
      }}

  """
  @spec info(binary, :struct) :: {:ok, info} | {:error, term}
  @spec info(binary, :keyword) :: {:ok, Keyword.t()} | {:error, term}
  def info(bin, style \\ :struct)

  # Compatibility with :elixir_uuid's info
  def info(bin, :keyword) when is_binary(bin) do
    with {:ok, uuid} <- parse(bin) do
      {:ok,
       [
         uuid: to_string(uuid),
         binary: uuid.bytes,
         type: uuid.format,
         version: uuid.version,
         variant: format_variant(uuid.variant)
       ]}
    end
  end

  def info(bin, :struct) when is_binary(bin) do
    parse(bin)
  end

  def info(_, style) when style in [:keyword, :struct],
    do: {:error, :invalid_format}

  @doc """
  Returns true if the given string is a valid UUID.

  ## Options

  * `strict: boolean`, if true, requires strict RFC 4122 conformance,
  i.e. version 6 is considered invalid
  """
  @spec valid?(binary) :: boolean
  @spec valid?(binary, Keyword.t()) :: boolean
  def valid?(bin, opts \\ [])

  def valid?(bin, opts) do
    strict? = Keyword.get(opts, :strict, false)

    case parse(bin) do
      {:ok, _} when not strict? ->
        true

      {:ok, %__MODULE__{version: 6}} when strict? ->
        false

      {:ok, _} ->
        true

      {:error, _} ->
        false
    end
  end

  @doc """
  Parses a `#{__MODULE__}` from a binary.

  Supported formats include human-readable strings, as well as
  the raw binary form of the UUID.

  ## Examples

      iex> {:ok, uuid} = Uniq.UUID.parse("f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
      {:ok, %Uniq.UUID{
        bytes: <<248, 29, 79, 174, 125, 236, 17, 208, 167, 101, 0, 160, 201, 30, 107, 246>>,
        format: :default,
        node: <<0, 160, 201, 30, 107, 246>>,
        seq: 10085,
        time: 130742845922168750,
        variant: <<2::size(2)>>,
        version: 1
      }}
      ...> {:ok, %Uniq.UUID{uuid | format: :urn}} == Uniq.UUID.parse("urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
      true

      iex> match?({:ok, %Uniq.UUID{format: :default, version: 1}}, Uniq.UUID.uuid1() |> Uniq.UUID.parse())
      true
  """
  @spec parse(binary) :: {:ok, info} | {:error, term}
  def parse(bin)

  def parse("urn:uuid:" <> uuid) do
    with {:ok, uuid} <- parse(uuid) do
      {:ok, %__MODULE__{uuid | format: :urn}}
    end
  end

  def parse(<<_::128>> = bin),
    do: parse_raw(bin, %__MODULE__{format: :raw})

  def parse(<<bin::bytes(32)>>) do
    with {:ok, raw} <- Base.decode16(bin, case: :mixed) do
      parse_raw(raw, %__MODULE__{format: :hex})
    else
      :error ->
        {:error, {:invalid_format, :hex}}
    end
  end

  def parse(<<a::bytes(8), ?-, b::bytes(4), ?-, c::bytes(4), ?-, d::bytes(4), ?-, e::bytes(12)>>) do
    with {:ok, bin} <- Base.decode16(a <> b <> c <> d <> e, case: :mixed) do
      parse_raw(bin, %__MODULE__{format: :default})
    else
      :error ->
        {:error, {:invalid_format, :default}}
    end
  end

  def parse(<<uuid::bytes(22)>>) do
    with {:ok, value} <- Base.url_decode64(uuid <> "==") do
      parse_raw(value, %__MODULE__{format: :slug})
    else
      _ ->
        {:error, {:invalid_format, :slug}}
    end
  end

  def parse(_bin), do: {:error, :invalid_format}

  # Parse version
  defp parse_raw(<<_::48, version::uint(4), _::bitstring>> = bin, acc) do
    case version do
      v when v in [1, 3, 4, 5, 6] ->
        with {:ok, uuid} <- parse_raw(version, bin, acc) do
          {:ok, %__MODULE__{uuid | bytes: bin}}
        end

      _ ->
        {:error, {:unknown_version, version}}
    end
  end

  # Parse variant
  defp parse_raw(version, <<time::64, @reserved_ncs, rest::bits(63)>>, acc),
    do: parse_raw(version, @reserved_ncs, time, rest, acc)

  defp parse_raw(version, <<time::64, @rfc_variant, rest::bits(62)>>, acc),
    do: parse_raw(version, @rfc_variant, time, rest, acc)

  defp parse_raw(version, <<time::64, @reserved_ms, rest::bits(61)>>, acc),
    do: parse_raw(version, @reserved_ms, time, rest, acc)

  defp parse_raw(version, <<time::64, @reserved_future, rest::bits(61)>>, acc),
    do: parse_raw(version, @reserved_future, time, rest, acc)

  defp parse_raw(_version, <<_time::64, variant::bits(3), _rest::bits(61)>>, _acc) do
    {:error, {:unknown_variant, variant}}
  end

  # Parses RFC 4122, version 1-5 uuids
  defp parse_raw(version, variant, time, rest, acc) when version < 6 do
    variant_size = bit_size(variant)
    clock_hi_size = 8 - variant_size
    clock_size = 8 + clock_hi_size

    with <<time_lo::bits(32), time_mid::bits(16), _version::4, time_hi::bits(12)>> <-
           <<time::64>>,
         <<timestamp::uint(60)>> <-
           <<time_hi::bits(12), time_mid::bits(16), time_lo::bits(32)>>,
         <<clock_hi::bits(clock_hi_size), clock_lo::bits(8), node::bits(48)>> <-
           rest,
         <<clock::uint(clock_size)>> <-
           <<clock_hi::bits(clock_hi_size), clock_lo::bits(8)>> do
      {:ok,
       %__MODULE__{
         acc
         | version: version,
           variant: variant,
           time: timestamp,
           seq: clock,
           node: node
       }}
    else
      other ->
        {:error, {:invalid_format, other, variant_size, clock_hi_size, clock_size}}
    end
  end

  # Parses proposed version 6 uuids, which are very much like version 1, but with some field ordering changes
  defp parse_raw(6, <<1::1, 0::1>> = variant, time, rest, acc) do
    with <<time_hi::bits(48), _version::4, time_lo::bits(12)>> <- time,
         <<timestamp::uint(60)>> <- <<time_hi::bits(48), time_lo::bits(12)>>,
         <<clock::uint(14), node::bits(48)>> <-
           rest do
      %__MODULE__{
        acc
        | version: 6,
          variant: variant,
          time: timestamp,
          seq: clock,
          node: node
      }
    else
      _ ->
        {:error, {:invalid_format, :v6}}
    end
  end

  defp parse_raw(6, variant, _time, _rest, _acc), do: {:error, {:invalid_variant, variant}}

  # Handles proposed version 7 and 8 uuids
  defp parse_raw(version, _variant, _time, _rest, _acc),
    do: {:error, {:unsupported_version, version}}

  @doc """
  Formats a `#{__MODULE__}` as a string, using the format it was originally generated with.

  See `to_string/2` if you want to specify what format to produce.
  """
  @spec to_string(t | info) :: String.t()
  def to_string(uuid)

  def to_string(<<raw::bits(128)>>),
    do: format(raw, :default)

  def to_string(%__MODULE__{bytes: raw, format: format}),
    do: format(raw, format)

  @doc """
  Same as `to_string/1`, except you can specify the desired format.

  The `format` can be one of the following:

  * `:default`, produces strings like `"f81d4fae-7dec-11d0-a765-00a0c91e6bf6"`
  * `:urn`, produces strings like `"urn:uuid:f81d4fae-7dec-11d0-a765-00a0c91e6bf6"`
  * `:hex`, produces strings like `"f81d4fae7dec11d0a76500a0c91e6bf6"`
  * `:slug`, produces strings like `"-B1Prn3sEdCnZQCgyR5r9g=="`
  * `:raw`, produces the raw binary encoding of the uuid in 128 bits
  """
  @spec to_string(t | info, format) :: String.t()
  def to_string(uuid, format)

  def to_string(<<raw::bits(128)>>, format) when format in @formats,
    do: format(raw, format)

  def to_string(%__MODULE__{bytes: raw}, format) when format in @formats,
    do: format(raw, format)

  @doc """
  This function takes a UUID string in any of the formats supported by `to_string/1`,
  and returns the raw, binary-encoded form.
  """
  @spec string_to_binary!(String.t()) :: t | no_return
  def string_to_binary!(str)

  def string_to_binary!(<<_::128>> = uuid), do: uuid

  def string_to_binary!(<<hex::bytes(32)>>) do
    :binary.decode_hex(hex)
  rescue
    _ ->
      raise ArgumentError, message: "invalid uuid string"
  end

  def string_to_binary!(
        <<a::bytes(8), ?-, b::bytes(4), ?-, c::bytes(4), ?-, d::bytes(4), ?-, e::bytes(12)>>
      ) do
    :binary.decode_hex(a <> b <> c <> d <> e)
  rescue
    _ ->
      raise ArgumentError, message: "invalid uuid string"
  end

  def string_to_binary!(<<slug::bytes(22)>>) do
    with {:ok, value} <- Base.url_decode64(slug <> "==") do
      value
    else
      _ ->
        raise ArgumentError, message: "invalid uuid string"
    end
  end

  def string_to_binary!(_) do
    raise ArgumentError, message: "invalid uuid string"
  end

  @doc """
  Compares two UUIDs, using their canonical 128-bit integer form, as described in RFC 4122.

  You may provide the UUIDs in either string, binary, or as a `Uniq.UUID` struct.
  """
  @spec compare(String.t() | info, String.t() | info) :: :lt | :eq | :gt
  def compare(a, b)

  def compare(%__MODULE__{} = a, %__MODULE__{} = b) do
    a = to_canonical_integer(a)
    b = to_canonical_integer(b)

    do_compare(a, b)
  end

  def compare(%__MODULE__{} = a, <<b::biguint(128)>>) do
    a = to_canonical_integer(a)

    do_compare(a, b)
  end

  def compare(%__MODULE__{} = a, b) when is_binary(b) do
    a = to_canonical_integer(a)
    b = string_to_binary!(b)

    do_compare(a, b)
  end

  def compare(<<a::biguint(128)>>, %__MODULE__{} = b) do
    b = to_canonical_integer(b)

    do_compare(a, b)
  end

  def compare(a, %__MODULE__{} = b) when is_binary(a) do
    a = string_to_binary!(a)
    b = to_canonical_integer(b)

    do_compare(a, b)
  end

  def compare(<<a::biguint(128)>>, <<b::biguint(128)>>),
    do: do_compare(a, b)

  def compare(a, b) when is_binary(a) and is_binary(b) do
    a = to_string(a)
    b = to_string(b)

    do_compare(a, b)
  end

  defp do_compare(a, b) do
    cond do
      a < b ->
        :lt

      a == b ->
        :eq

      :else ->
        :gt
    end
  end

  defp to_canonical_integer(%__MODULE__{bytes: <<value::biguint(128)>>}) do
    value
  end

  defp format(raw, :raw), do: raw
  defp format(raw, :default), do: format_default(raw)
  defp format(raw, :hex), do: IO.iodata_to_binary(for <<bs::4 <- raw>>, do: c(bs))
  defp format(raw, :urn), do: "urn:uuid:#{format(raw, :default)}"
  defp format(raw, :slug), do: Base.url_encode64(raw, padding: false)

  defp format_default(<<
         a1::4,
         a2::4,
         a3::4,
         a4::4,
         a5::4,
         a6::4,
         a7::4,
         a8::4,
         b1::4,
         b2::4,
         b3::4,
         b4::4,
         c1::4,
         c2::4,
         c3::4,
         c4::4,
         d1::4,
         d2::4,
         d3::4,
         d4::4,
         e1::4,
         e2::4,
         e3::4,
         e4::4,
         e5::4,
         e6::4,
         e7::4,
         e8::4,
         e9::4,
         e10::4,
         e11::4,
         e12::4
       >>) do
    <<c(a1), c(a2), c(a3), c(a4), c(a5), c(a6), c(a7), c(a8), ?-, c(b1), c(b2), c(b3), c(b4), ?-,
      c(c1), c(c2), c(c3), c(c4), ?-, c(d1), c(d2), c(d3), c(d4), ?-, c(e1), c(e2), c(e3), c(e4),
      c(e5), c(e6), c(e7), c(e8), c(e9), c(e10), c(e11), c(e12)>>
  end

  defp c(0), do: ?0
  defp c(1), do: ?1
  defp c(2), do: ?2
  defp c(3), do: ?3
  defp c(4), do: ?4
  defp c(5), do: ?5
  defp c(6), do: ?6
  defp c(7), do: ?7
  defp c(8), do: ?8
  defp c(9), do: ?9
  defp c(10), do: ?a
  defp c(11), do: ?b
  defp c(12), do: ?c
  defp c(13), do: ?d
  defp c(14), do: ?e
  defp c(15), do: ?f

  defp format_variant(@reserved_future), do: :reserved_future
  defp format_variant(@reserved_ms), do: :reserved_microsoft
  defp format_variant(@rfc_variant), do: :rfc4122
  defp format_variant(@reserved_ncs), do: :reserved_ncs
  defp format_variant(_), do: :unknown

  defp mac_address do
    candidate_interface? = fn {_if, info} ->
      flags = Keyword.get(info, :flags, [])

      Enum.member?(flags, :up) and Enum.member?(flags, :broadcast) and
        Keyword.has_key?(info, :hwaddr)
    end

    with {:ok, interfaces} <- :inet.getifaddrs(),
         {_if, info} <- Enum.find(interfaces, candidate_interface?) do
      IO.iodata_to_binary(info[:hwaddr])
    else
      _ ->
        # In lieu of a MAC address, we can generate an equivalent number of random bytes
        <<head::7, _::1, tail::46>> = :crypto.strong_rand_bytes(6)
        # Ensure the multicast bit is set, as per RFC 4122
        <<head::7, 1::1, tail::46>>
    end
  end

  defp hash(:md5, data), do: :crypto.hash(:md5, data)
  defp hash(:sha, data), do: :binary.part(:crypto.hash(:sha, data), 0, 16)

  @predefined_namespace_id Base.decode16!("6ba7b8109dad11d180b400c04fd430c8", case: :mixed)
  @nil_id <<0::128>>

  defp namespace_id(ns) when ns in [:dns, :url, :oid, :x500], do: @predefined_namespace_id
  defp namespace_id(nil), do: @nil_id

  defp namespace_id(<<_::128>> = ns), do: ns

  defp namespace_id(<<ns::bytes(32)>>) do
    with {:ok, raw} <- Base.decode16(ns, case: :mixed) do
      raw
    else
      _ ->
        invalid_namespace!()
    end
  end

  defp namespace_id(
         <<a::bytes(8), ?-, b::bytes(4), ?-, c::bytes(4), ?-, d::bytes(4), ?-, e::bytes(12)>>
       ) do
    with {:ok, raw} <- Base.decode16(a <> b <> c <> d <> e, case: :mixed) do
      raw
    else
      _ ->
        invalid_namespace!()
    end
  end

  defp namespace_id(<<ns::bytes(22)>>) do
    with {:ok, raw} <- Base.url_decode64(ns <> "==") do
      raw
    else
      _ ->
        invalid_namespace!()
    end
  end

  defp namespace_id(_ns), do: invalid_namespace!()

  defp invalid_namespace!,
    do:
      raise(ArgumentError,
        message: "expected a valid namespace atom (:dns, :url, :oid, :x500), or a UUID string"
      )

  ## Ecto

  if Code.ensure_loaded?(Ecto.Type) do
    use Ecto.Type

    @doc false
    @impl Ecto.Type
    def type, do: :binary_id

    @doc false
    @impl Ecto.Type
    def autogenerate(), do: uuid4()

    @doc false
    @impl Ecto.Type
    def cast(data)

    def cast(<<_::128>> = uuid),
      do: {:ok, uuid}

    def cast(uuid) when is_binary(uuid) do
      {:ok, string_to_binary!(uuid)}
    rescue
      ArgumentError ->
        :error
    end

    def cast(%__MODULE__{} = uuid),
      do: {:ok, to_string(uuid)}

    def cast(_), do: :error

    @doc false
    @impl Ecto.Type
    def load(<<_::128>> = uuid), do: {:ok, to_string(uuid)}

    def load(uuid) when is_binary(uuid) do
      {:ok, string_to_binary!(uuid)}
    rescue
      ArgumentError ->
        :error
    end

    @doc false
    @impl Ecto.Type
    def dump(%__MODULE__{bytes: uuid}), do: {:ok, uuid}
    def dump(<<_::128>> = uuid), do: {:ok, uuid}

    def dump(uuid) when is_binary(uuid) do
      {:ok, string_to_binary!(uuid)}
    rescue
      ArgumentError ->
        :error
    end

    @doc false
    @impl Ecto.Type
    def embed_as(_format), do: :self

    @doc false
    @impl Ecto.Type
    def equal?(a, b)

    def equal?(nil, _), do: false
    def equal?(_, nil), do: false
    def equal?(a, b, _params), do: compare(a, b) == :eq
  end

  defimpl String.Chars do
    alias Uniq.UUID

    def to_string(uuid), do: UUID.to_string(uuid)
  end
end
