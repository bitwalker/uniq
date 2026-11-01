defmodule Uniq.WithoutEctoTest do
  use ExUnit.Case, async: true

  @tag :tmp_dir
  test "compiles without warnings and generates UUIDs without Ecto", %{tmp_dir: tmp_dir} do
    sources = Path.expand("../lib", __DIR__) |> Path.join("*.ex") |> Path.wildcard()

    {output, status} =
      System.cmd("elixirc", ["--warnings-as-errors", "-o", tmp_dir | sources],
        env: [{"ERL_LIBS", nil}],
        stderr_to_stdout: true
      )

    assert status == 0, output
    assert output == ""

    {output, status} =
      System.cmd(
        "elixir",
        [
          "-pa",
          tmp_dir,
          "-e",
          """
          false = Code.ensure_loaded?(Ecto.ParameterizedType)
          {:ok, _} = Uniq.Generator.start_link([])
          for version <- [1, 4, 6, 7] do
            uuid = apply(Uniq.UUID, String.to_atom("uuid" <> Integer.to_string(version)), [])
            {:ok, %{version: ^version}} = Uniq.UUID.parse(uuid)
          end
          """
        ],
        env: [{"ERL_LIBS", nil}],
        stderr_to_stdout: true
      )

    assert status == 0, output
  end
end
