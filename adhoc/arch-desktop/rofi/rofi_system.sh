#!/usr/bin/env bash

set -euCo pipefail

function main() {
  local -Ar menu=(
    ['Poweroff (hibernate)']='systemctl hibernate'
  )

  local -r IFS=$'\n'
  # with some arguments:  execute a command mapped to $1
  # without any arguments show keys
  [[ $# -ne 0 ]] && eval "${menu[$1]}" || echo "${!menu[*]}"
}

main $@
