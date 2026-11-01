defmodule Uniq.UUIDComparisonTest do
  use ExUnit.Case, async: true

  alias Uniq.UUID

  @nil_uuid "00000000-0000-0000-0000-000000000000"
  @smaller "018dd112-d7d4-72fb-9650-558ff08be388"
  @larger "018dd112-d7d4-7f7a-80fd-7bf8daeab8ce"

  test "identical UUIDs compare equal in every representation" do
    for uuid <- [@nil_uuid, @smaller, @larger],
        a <- representations(uuid),
        b <- representations(uuid) do
      assert UUID.compare(a, b) == :eq
      assert UUID.equal?(a, b, nil)
    end
  end

  test "ordering is independent of representation and argument direction" do
    for {smaller, larger} <- [
          {@nil_uuid, @smaller},
          {@smaller, @larger},
          {@nil_uuid, @larger}
        ],
        a <- representations(smaller),
        b <- representations(larger) do
      assert UUID.compare(a, b) == :lt
      assert UUID.compare(b, a) == :gt
      refute UUID.equal?(a, b, nil)
    end
  end

  test "sorts same-millisecond UUIDv7 values by their encoded value" do
    ids = [
      "018dd112-d7d4-7a48-a433-fcb3294b9fb3",
      @larger,
      @smaller,
      "018dd112-d7d4-7348-8dd5-e7a8fed42a22"
    ]

    assert Enum.sort(ids, UUID) == [
             @smaller,
             "018dd112-d7d4-7348-8dd5-e7a8fed42a22",
             "018dd112-d7d4-7a48-a433-fcb3294b9fb3",
             @larger
           ]
  end

  test "invalid strings still raise rather than participating in term ordering" do
    for uuid <- [@smaller, UUID.info!(@smaller)] do
      assert_raise ArgumentError, fn -> UUID.compare(uuid, "invalid") end
      assert_raise ArgumentError, fn -> UUID.compare("invalid", uuid) end
    end
  end

  defp representations(uuid) do
    hex = String.replace(uuid, "-", "")
    raw = Base.decode16!(hex, case: :lower)
    {:ok, info} = UUID.parse(uuid)

    [
      uuid,
      String.upcase(uuid),
      hex,
      String.upcase(hex),
      raw,
      "urn:uuid:" <> uuid,
      Base.url_encode64(raw, padding: false),
      info
    ]
  end
end
