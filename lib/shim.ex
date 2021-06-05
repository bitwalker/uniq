unless Code.ensure_loaded?(UUID) do
  defmodule UUID do
    @moduledoc """
    This is a shim for the `UUID` module provided by the elixir_uuid library,
    to allow for drop-in integration with legacy code.
    """

    def info(uuid), do: Uniq.UUID.info(uuid, :keyword)

    def info!(uuid), do: Uniq.UUID.info!(uuid, :keyword)

    defdelegate binary_to_string(uuid), to: Uniq.UUID, as: :to_string

    defdelegate string_to_binary!(uuid), to: Uniq.UUID

    defdelegate uuid1(format \\ :default), to: Uniq.UUID

    defdelegate uuid1(clock_seq, node, format \\ :default), to: Uniq.UUID

    defdelegate uuid3(namespace_or_uuid, name, format \\ :default), to: Uniq.UUID

    defdelegate uuid4(format \\ :default), to: Uniq.UUID

    defdelegate uuid5(namespace_or_uuid, name, format \\ :default), to: Uniq.UUID

    # Strictly speaking, these aren't provided by elixir_uuid, but since people may
    # be likely to use this shim to access uniq, it makes sense to expose the additional
    # functionality
    defdelegate valid?(string), to: Uniq.UUID

    defdelegate uuid6(format \\ :default), to: Uniq.UUID

    defdelegate to_string(uuid), to: Uniq.UUID

    defdelegate to_string(uuid, format), to: Uniq.UUID
  end
end
