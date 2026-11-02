Mix.start()
Code.require_file("mix.exs")

fail = fn message ->
  IO.puts(:stderr, message)
  System.halt(1)
end

tag = System.get_env("RELEASE_TAG", "")
version = String.replace_prefix(tag, "v", "")
package_version = Mix.Project.config()[:version]

case Version.parse(version) do
  {:ok, %Version{pre: [], build: nil}} -> :ok
  _ -> fail.("Release tag must be a stable version without build metadata: #{inspect(tag)}")
end

if version != package_version do
  fail.("Release tag #{inspect(tag)} does not match package version #{package_version}")
end

if System.get_env("RELEASE_PRERELEASE", "false") != "false" do
  fail.("Prereleases must not publish through the stable release workflow")
end

IO.puts("Validated release #{tag} for package #{package_version}")
