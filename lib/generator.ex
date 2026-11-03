defmodule Uniq.Generator do
  @moduledoc """
  This module is used to interact with the global state
  needed to correctly generate version 1 and version 6 UUIDs.

  This state consists of an atomic clock sequence value.

  On init, the clock sequence is seeded with a random value provided
  by interpreting bits from the cryptographic random number generator
  as an unsigned 14 bit integer.

  The sequence is incremented for every generation, including when timestamps
  repeat or the wall clock moves backwards. This does not make timestamps
  monotonic.
  """
  use GenServer

  @compile {:inline, [get_atomic_ref: 0, now: 0]}

  @clock_index 1

  @doc """
  Generates the next initial state for UUID creation.
  """
  def next do
    ref = get_atomic_ref()
    current_ts = now()
    clock = :atomics.add_get(ref, @clock_index, 1)
    {current_ts, clock}
  end

  @doc false
  def timestamp_from_unix(microseconds) when is_integer(microseconds),
    do: microseconds * 10 + 122_192_928_000_000_000

  ## Private

  defp get_atomic_ref do
    # By storing the atomic ref in the process state, we avoid all the
    # overhead of looking it up in an ETS table, or asking a process for it,
    # and it becomes almost as efficient as a thread local
    case Process.get(__MODULE__) do
      nil ->
        # The slow path, in this case we have to fetch the ref from the public ETS table
        [{_, ref}] = :ets.lookup(__MODULE__, :counters)
        Process.put(__MODULE__, ref)
        ref

      ref ->
        # The fast path
        ref
    end
  end

  # UUIDv1/v6 count 100 ns intervals since 1582-10-15 (RFC 9562 Appendix A).
  defp now do
    System.system_time(:microsecond) |> timestamp_from_unix()
  end

  ## GenServer Implementation

  def start_link(_args), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  def init(_) do
    ref = :atomics.new(1, signed: false)

    # Seed the clock index
    <<clock::unsigned-integer-size(14), _::2>> = :crypto.strong_rand_bytes(2)
    :atomics.put(ref, @clock_index, clock)

    # Store the atomic ref in a public ETS table to avoid bottlenecks
    tab = :ets.new(__MODULE__, [:public, :named_table, {:read_concurrency, true}])

    :ets.insert(tab, {:counters, ref})

    {:ok, tab}
  end
end
