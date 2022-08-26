defmodule Uniq.Macros do
  defmacro defextension(module, do: body) do
    module = Macro.expand(module, __ENV__)

    if Code.ensure_loaded?(module) do
      quote do
        unquote(body)
      end
    end
  end
end
