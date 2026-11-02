false = Code.ensure_loaded?(Ecto.ParameterizedType)
{:ok, _} = Application.ensure_all_started(:uniq)

for {generator, version} <- [uuid1: 1, uuid4: 4, uuid6: 6, uuid7: 7] do
  uuid = apply(Uniq.UUID, generator, [])
  {:ok, %Uniq.UUID{version: ^version} = info} = Uniq.UUID.parse(uuid)
  :eq = Uniq.UUID.compare(info, uuid)
  :eq = Uniq.UUID.compare(uuid, Uniq.UUID.to_string(uuid, :urn))
end

IO.puts("Package consumer without Ecto: ok")
