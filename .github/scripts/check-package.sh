#!/usr/bin/env bash
set -euo pipefail

mode="${1:-all}"
case "${mode}" in
  core|all) ;;
  *) echo "Usage: bash .github/scripts/check-package.sh [core|all]" >&2; exit 64 ;;
esac

repo_root="$(pwd)"
work_dir="$(mktemp -d)"
trap 'rm -rf -- "${work_dir}"' EXIT

mix hex.build --unpack --output "${work_dir}/package"

check_consumer() (
  trap - EXIT
  fixture="$1"
  requirement="${2:-}"
  consumer="${work_dir}/${fixture}-${requirement//[^a-zA-Z0-9]/_}"
  mkdir -p "${consumer}"
  cp "${repo_root}/.github/consumers/${fixture}/"* "${consumer}/"
  cd "${consumer}"
  unset ERL_LIBS
  export MIX_ENV=dev MIX_BUILD_PATH="${consumer}/_build" MIX_DEPS_PATH="${consumer}/deps"
  export UNIQ_PACKAGE="${work_dir}/package" ECTO_REQUIREMENT="${requirement}"
  mix deps.get
  mix compile --warnings-as-errors
  mix run --no-compile smoke.exs
)

check_consumer core
if [[ "${mode}" == all ]]; then
  check_consumer ecto '3.12.0'
  check_consumer ecto '~> 3.0'
fi
