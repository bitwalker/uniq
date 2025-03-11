defmodule Uniq.Ecto.Test do
  use ExUnit.Case, async: true

  alias Ecto.TestRepo
  alias Uniq.UUID

  defmodule Person.Version4 do
    use Ecto.Schema

    @primary_key {:id, Uniq.UUID, autogenerate: true}
    schema "person_v4" do
      field(:name, :string)
    end
  end

  defmodule Person.Version5 do
    use Ecto.Schema

    @namespace UUID.uuid5(:dns, "person.v5.uniq.example.com", :raw)

    @primary_key {:id, Uniq.UUID, version: 5, namespace: @namespace, autogenerate: true}
    schema "person_v5" do
      field(:name, :string)
    end
  end

  defmodule Person.Version6 do
    use Ecto.Schema

    @primary_key false
    schema "person_v6" do
      field(:id, Uniq.UUID, version: 6, autogenerate: true)
      field(:name, :string)
    end
  end

  defmodule Person.Version7 do
    use Ecto.Schema

    @primary_key false
    schema "person_v7" do
      field(:id, Uniq.UUID, version: 7, autogenerate: true)
      field(:name, :string)
    end
  end

  test "can autogenerate primary keys" do
    assert %Person.Version4{id: uuid} =
             %Person.Version4{}
             |> Ecto.Changeset.cast(%{name: "Paul"}, [:name])
             |> TestRepo.insert!()

    assert {:ok, %UUID{version: 4}} = UUID.parse(uuid)

    assert %Person.Version5{id: uuid} =
             %Person.Version5{}
             |> Ecto.Changeset.cast(%{name: "Paul"}, [:name])
             |> TestRepo.insert!()

    assert {:ok, %UUID{version: 5}} = UUID.parse(uuid)

    assert %Person.Version6{id: uuid} =
             %Person.Version6{}
             |> Ecto.Changeset.cast(%{name: "Paul"}, [:name])
             |> TestRepo.insert!()

    assert {:ok, %UUID{version: 6}} = UUID.parse(uuid)

    assert %Person.Version7{id: uuid} =
             %Person.Version7{}
             |> Ecto.Changeset.cast(%{name: "Paul"}, [:name])
             |> TestRepo.insert!()

    assert {:ok, %UUID{version: 7}} = UUID.parse(uuid)
  end
end
