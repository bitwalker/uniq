# CI maintenance

`elixir.yml` and `release.yml` call the same build and security workflows.
Every PR, including fork PRs, runs these checks without publication secrets.
The security workflow also runs weekly and on manual dispatch. All checkouts
use the event's exact commit and disable credential persistence. Jobs have
explicit permissions and timeouts; small builds use no dependency caches.

## Compatibility checks

| Check | Purpose |
| --- | --- |
| Elixir 1.16.3 / OTP 26.2.5.21 | Strict compilation and tests |
| Elixir 1.18.4 / OTP 27.3.4.19 | Strict compilation and tests |
| Elixir 1.20.5 / OTP 29.1.2 | Strict compilation and tests |
| Elixir 1.13.4 / OTP 24.3.4.17 | Packaged core consumer without Ecto |
| Format, docs, and package consumers | Formatting, lock hygiene, Hex audit, strict docs, packaged consumers |

The minimum compiler is tested without optional Ecto or documentation tooling.
Package consumers compile fresh unpacked Hex contents outside this checkout,
with separate dependency and build directories. Ecto consumers exercise schema
primary/foreign keys against Ecto 3.12.0 and the newest allowed 3.x version.
3.12 introduced the parameterized-type representation used by ScopedUUID and is
the declared minimum for the optional Ecto integration (`~> 3.12`).
Historical Ecto 3.12 resolves Decimal 2.x, which currently has security advisories;
that isolated compatibility fixture is never published. The repository's locked
dependencies are audited separately.

On an appropriate runtime (use mise locally), reproduce the build gates with:

```sh
MIX_ENV=test mix deps.get --only test --check-locked
MIX_ENV=test mix deps.compile
MIX_ENV=test mix compile --warnings-as-errors
MIX_ENV=test mix test --warnings-as-errors
# Fetch every environment so the unused-lock check can traverse the whole graph.
MIX_ENV=docs mix deps.get --check-locked
MIX_ENV=docs mix deps.unlock --check-unused
mix format --check-formatted
MIX_ENV=docs mix hex.audit
MIX_ENV=docs mix docs --warnings-as-errors
bash .github/scripts/check-package.sh
# On Elixir 1.13 / OTP 24:
bash .github/scripts/check-package.sh core
```

## Workflow dependencies and lint

Remote actions stay pinned to full commit SHAs and containers to digests.
`actions.lock` records direct and transitive action commits and repository
identities. GitHub [announced general availability across plans](https://github.blog/changelog/2026-10-28-github-actions-workflow-lockfiles-are-generally-available/),
including automatic Dependabot lockfile updates. A hosted pilot is still needed
for this repository; full-SHA references provide enforcement independently and
zizmor's pinning checks stay enabled. Containers and downloaded tools need their own pins.

The security workflow uses gh-actions-lock **0.4.0**, actionlint **1.7.12**
(with ShellCheck), and zizmor **1.31.1** through zizmor-action **0.8.0**.
The lockfile verifier binary is SHA-256 checked before execution; the zizmor
action installs hash-verified wheels. See `security.yml` for exact action SHAs,
the actionlint container digest, and the Linux binary checksum. No zizmor rules
are disabled. `.github/actionlint.yaml` narrowly accommodates GitHub's `$/`
self-repository workflow syntax until actionlint supports it.

With matching versions installed, run:

```sh
actionlint
shellcheck .github/scripts/*.sh
zizmor .github
gh actions-lock --verify-local  # offline coverage only
gh actions-lock --verify        # authenticated online provenance check
```

Use gh-actions-lock 0.4.0 for these commands; an older installed gh extension
may implement a different lockfile format. The standalone release binary accepts
the same flags. For macOS arm64, download the `darwin-arm64` asset from
the [0.4.0 release](https://github.com/github/gh-actions-lock/releases/tag/v0.4.0)
and verify SHA-256
`5934d40f8cc78ed3e9be15ba43fc82c2432cbf3aa088107f923878df07099bec`
before executing it.

Dependabot checks Actions and Mix weekly, with a seven-day cooldown for version
updates (security updates bypass cooldown). Mix updates change only the lockfile.
Review upstream changes before accepting action updates. After changing workflow
references, regenerate using `gh actions-lock --no-narrow` so full SHAs remain
in YAML; the default command can rewrite SHAs to tags. Inspect both YAML and
lockfile changes, then run both verifier modes and the linters. When removing an
action, delete the generated lockfile and regenerate to remove obsolete entries.
Never use `--accept-moved` automatically. If Dependabot does not refresh the
lockfile, a maintainer must regenerate it before merging. Its first hosted PR
should establish that behavior for this repository.

## Release validation and rollout

Published releases run all shared checks against `github.sha`, the commit of
the release event. A stable tag must exactly match `mix.exs`, with an optional
single `v` prefix; prerelease tags and build metadata are rejected. Tag metadata
is passed through environment variables. Publication rechecks the version and
builds docs and the package from that same commit and frozen dependency lock.
The Hex key exists only on the final publication step. Publishing is serialized
across release runs and is never cancelled by CI concurrency settings.

Rehearse locally without publishing:

```sh
RELEASE_TAG=0.6.4 RELEASE_PRERELEASE=false elixir .github/scripts/check-release.exs
MIX_ENV=test mix test test/ci_release_test.exs
```

The Release workflow's manual dispatch runs validation only: select the desired
release commit/ref and supply its version tag. Manual dispatch never publishes.
Only a stable `release: published` event can enter the publication job.

After the first hosted PR and manual release rehearsal succeed, configure the
repository ruleset to require the build and security checks shown by those runs
(three runtime tests, minimum core consumer, package/docs, Actions lockfile,
actionlint/ShellCheck, and zizmor). Do not path-filter these required workflows.
Configure the `hex-publish` environment with appropriate required reviewers and
release-tag restrictions, and provide a package-scoped `HEX_API_KEY` there.
Review the actual Hex key's permissions. These hosted settings and native lockfile
enforcement require separate verification; changing YAML does not configure them.
