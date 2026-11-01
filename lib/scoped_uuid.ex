defmodule Uniq.ScopedUUID do
  import Uniq.Macros, only: [defextension: 2]

  @moduledoc """
  Generates and validates UUIDs in Ecto schemas which are scoped using a prefix.

  With scoped UUIDs, applications can differentiate between UUIDs that are generated
  by and intended for use in different parts of the application, preventing
  their use in other scopes.

  It also makes it easy to determine the intended use of an ID in error logs,
  in client code, etc.

  The scope is included in the Ecto field definition, and is prepended to the UUID
  which is separated from the ID itself by an `_`. The UUID itself is rendered
  as a slug making it easy to use outside the application, e.g.: `account_AYilFryMfFqbaBJlH1WLng`.

  Only the UUID itself is stored in the Ecto store itself as a standard binary
  ID, with the scope used for validation in the application.

  Foreign keys in schemas that use a `belongs_to` will automatically use the
  correct scoping as well.

  When defining a ScopedUUID field, the following options are supported:

    * scope: String.t(), required. Must not contain underscores.
    * uuid_version: integer, defaults to 7. The version of UUIDs to generate.

  ## Examples

      @primary_key {:id, Uniq.ScopedUUID, scope: "user", autogenerate: true}
      @foreign_key_type Uniq.ScopedUUID

      schema "locations" do
        field(:external_id, Uniq.ScopedUUID, scope: "placeext")
        belongs_to(:region, MyApp.Region) # automatically scoped!
      end

  """

  defextension Ecto.ParameterizedType do
    use Ecto.ParameterizedType

    @impl true
    def init(opts) do
      schema = Keyword.fetch!(opts, :schema)
      field = Keyword.fetch!(opts, :field)
      version = Keyword.get(opts, :uuid_version, 7)

      uuid_config =
        Uniq.UUID.init(
          schema: schema,
          field: field,
          version: version,
          format: :slug,
          default: :raw,
          dump: :raw
        )

      foreign_key? = Keyword.get(opts, :foreign_key) != nil

      if foreign_key? do
        %{
          schema: schema,
          field: field,
          uuid_config: uuid_config
        }
      else
        scope = Keyword.get(opts, :scope) || raise "`:scope` option is required"

        if String.contains?(scope, "_") do
          raise "ScopedUUID scopes may not contain underscores."
        end

        %{
          scope: scope,
          uuid_config: uuid_config
        }
      end
    end

    @impl true
    def type(_params), do: :uuid

    @impl true
    def cast(nil, _params), do: {:ok, nil}

    def cast(data, params) do
      scope = scope_for(params)

      case extract_uuid(data, params) do
        {:ok, ^scope, _uuid} -> {:ok, data}
        _error -> :error
      end
    end

    @impl true
    def load(data, loader, params) do
      case Uniq.UUID.load(data, loader, params.uuid_config) do
        {:ok, nil} -> {:ok, nil}
        {:ok, uuid} -> {:ok, add_scope(uuid, params)}
        :error -> :error
      end
    end

    @impl true
    def dump(nil, _, _), do: {:ok, nil}

    def dump(slug, _dumper, params) do
      case extract_uuid(slug, params) do
        {:ok, _scope, uuid} -> {:ok, uuid}
        :error -> :error
      end
    end

    @impl true
    def autogenerate(params) do
      params.uuid_config
      |> Uniq.UUID.autogenerate()
      |> add_scope(params)
    end

    @spec generate(module, field :: atom) ::
            {:ok, scoped_uuid :: String.t()} | {:error, reason :: String.t()}
    @doc "Generates an ID for a schema module and field, by default the id field"
    def generate(module, field \\ :id) do
      try do
        {:parameterized, {Uniq.ScopedUUID, params}} =
          Map.get(module.__changeset__(), field)

        {:ok, autogenerate(params)}
      rescue
        _ ->
          {:error, "Can only generate IDs for ScopedUUID fields"}
      end
    end

    @impl true
    def embed_as(format, params), do: Uniq.UUID.embed_as(format, params.uuid_config)

    @impl true
    def equal?(nil, nil, _params), do: true
    def equal?(nil, _b, _params), do: false
    def equal?(_a, nil, _params), do: false

    def equal?(a, b, params) do
      with {:ok, scope, uuid_a} <- extract_uuid(a, params),
           {:ok, ^scope, uuid_b} <- extract_uuid(b, params) do
        Uniq.UUID.equal?(uuid_a, uuid_b, params.uuid_config)
      else
        _ -> Uniq.UUID.equal?(a, b, params.uuid_config)
      end
    end

    defp scope_for(%{scope: scope}), do: scope

    # When dealing with a belongs_to association, fetch the scope from
    # the association's schema module
    defp scope_for(%{schema: schema, field: field}) do
      %{related: schema, related_key: field} = schema.__schema__(:association, field)
      {:parameterized, {__MODULE__, %{scope: scope}}} = schema.__schema__(:type, field)
      scope
    end

    defp add_scope(uuid, params) do
      scope = scope_for(params)
      "#{scope}_#{uuid}"
    end

    defp fake_dumper(_type, value), do: value

    defp extract_uuid(scoped_uuid, params) do
      scope = scope_for(params)

      with <<^scope::binary, ?_, slug::binary>> <- scoped_uuid,
           {:ok, uuid} <- Uniq.UUID.dump(slug, &fake_dumper/2, params.uuid_config) do
        {:ok, scope, uuid}
      else
        _ -> :error
      end
    end
  end
end
