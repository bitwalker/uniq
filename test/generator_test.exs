defmodule Uniq.Generator.Test do
  use ExUnit.Case

  test "generator state is always unique even under parallel generations" do
    :ets.new(:result, [:named_table, :public, :set, {:keypos, 1}])

    parent = self()

    tasks =
      Enum.map(1..System.schedulers_online(), fn id ->
        {spawn(fn ->
           start = System.monotonic_time(:millisecond)
           repeat_on_generating(parent, id, start)
         end), id}
      end)

    assert duplicates(tasks) == []
  end

  defp repeat_on_generating(parent, id, start) do
    result = Uniq.Generator.next()

    if !:ets.insert_new(:result, {result, id}) do
      [{prev, prev_id}] = :ets.lookup(:result, result)
      send(parent, {:duplicate, id, prev, prev_id})
    else
      t = System.monotonic_time(:millisecond)

      if t - start > 15_000 do
        send(parent, {:done, id})
      else
        repeat_on_generating(parent, id, start)
      end
    end
  end

  defp duplicates(tasks), do: duplicates(tasks, [])
  defp duplicates([], acc), do: acc

  defp duplicates([{_, id} | tasks], acc) do
    receive do
      {:duplicate, ^id, gen, conflicting_id} ->
        duplicates(tasks, [{gen, id, conflicting_id} | acc])

      {:done, ^id} ->
        duplicates(tasks, acc)
    end
  end
end
