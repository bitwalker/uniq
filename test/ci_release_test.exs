defmodule Uniq.CIReleaseTest do
  use ExUnit.Case, async: true

  @version Mix.Project.config()[:version]
  @script Path.expand("../.github/scripts/check-release.exs", __DIR__)

  test "accepts the exact stable version with or without a v prefix" do
    for tag <- [@version, "v" <> @version] do
      {output, status} = check(tag)
      assert status == 0, output
    end
  end

  test "rejects a tag that does not match the package version" do
    version = Version.parse!(@version)
    {output, status} = check("#{version.major + 1}.#{version.minor}.#{version.patch}")
    assert status != 0, output
  end

  test "rejects malformed, prerelease, and build-metadata tags" do
    for tag <- ["", "vv" <> @version, @version <> "-rc.1", @version <> "+build", "$(exit 0)"] do
      {output, status} = check(tag)
      assert status != 0, output
    end
  end

  test "rejects a release marked as prerelease even with a stable tag" do
    {output, status} = check(@version, "true")
    assert status != 0, output
  end

  defp check(tag, prerelease \\ "false") do
    System.cmd("elixir", [@script],
      env: [{"RELEASE_TAG", tag}, {"RELEASE_PRERELEASE", prerelease}],
      stderr_to_stdout: true
    )
  end
end
