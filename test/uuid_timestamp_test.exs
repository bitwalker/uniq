defmodule Uniq.UUIDTimestampTest do
  use ExUnit.Case, async: true

  alias Uniq.{Generator, UUID}

  test "converts Unix microseconds to UUID ticks across fractional boundaries" do
    # RFC 9562 Appendix A provides the epoch offset and contemporary vector.
    for {unix_us, ticks} <- [
          {-12_219_292_800_000_000, 0},
          {-1, 122_192_927_999_999_990},
          {0, 122_192_928_000_000_000},
          {99_999, 122_192_928_000_999_990},
          {100_000, 122_192_928_001_000_000},
          {999_999, 122_192_928_009_999_990},
          {1_000_000, 122_192_928_010_000_000},
          {1_645_557_742_000_000, 138_648_505_420_000_000}
        ] do
      assert Generator.timestamp_from_unix(unix_us) == ticks
    end
  end

  test "generates v1 and v6 timestamps within the wall-clock interval" do
    node = <<1, 2, 3, 4, 5, 6>>

    for generate <- [
          fn -> UUID.uuid1(:raw) end,
          fn -> UUID.uuid1(1234, node, :raw) end,
          fn -> UUID.uuid6(:raw) end
        ] do
      before = System.system_time(:microsecond)
      raw = generate.()
      after_time = System.system_time(:microsecond)
      {:ok, info} = UUID.parse(raw)

      # A backwards clock adjustment reverses the endpoints, not the epoch.
      lower = min(before, after_time) * 10 + 122_192_928_000_000_000
      upper = max(before, after_time) * 10 + 122_192_928_000_000_000
      assert info.time >= lower
      assert info.time <= upper
      assert info.time < 1_152_921_504_606_846_976
    end
  end

  test "decodes the independent RFC 9562 v1 and v6 timestamp vectors" do
    for uuid <- [
          "C232AB00-9414-11EC-B3C8-9F6BDECED846",
          "1EC9414C-232A-6B00-B3C8-9F6BDECED846"
        ] do
      assert {:ok,
              %UUID{
                time: 138_648_505_420_000_000,
                seq: 0x33C8,
                node: <<0x9F, 0x6B, 0xDE, 0xCE, 0xD8, 0x46>>
              }} = UUID.parse(uuid)
    end
  end
end
