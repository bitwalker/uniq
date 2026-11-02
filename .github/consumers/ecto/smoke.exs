defmodule Account do
  use Ecto.Schema
  @primary_key {:id, Uniq.ScopedUUID, scope: "account", autogenerate: true}
  schema "accounts" do
  end
end

defmodule Post do
  use Ecto.Schema
  @foreign_key_type Uniq.ScopedUUID
  schema "posts" do
    belongs_to(:account, Account)
  end
end

{:parameterized, {Uniq.ScopedUUID, params}} = Account.__schema__(:type, :id)
id = Uniq.ScopedUUID.autogenerate(params)
{:ok, raw} = Uniq.ScopedUUID.dump(id, nil, params)
{:ok, ^id} = Uniq.ScopedUUID.load(raw, nil, params)

{:parameterized, {Uniq.ScopedUUID, foreign_params}} = Post.__schema__(:type, :account_id)
{:ok, ^id} = Uniq.ScopedUUID.cast(id, foreign_params)
{:ok, ^raw} = Uniq.ScopedUUID.dump(id, nil, foreign_params)

uuid = Uniq.UUID.uuid7()
{:ok, info} = Uniq.UUID.parse(uuid)
true = Uniq.UUID.equal?(info, uuid, nil)

IO.puts("Package consumer with Ecto #{Application.spec(:ecto, :vsn)}: ok")
