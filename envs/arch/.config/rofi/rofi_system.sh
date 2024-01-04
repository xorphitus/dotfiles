#!/usr/bin/env bash

set -euCo pipefail

function main() {
  # can not use some special charactors
  # e.g. ( )
  local -Ar menu=(
    ['Lock']="$HOME/dotfiles/envs/arch/bin/lock.sh"
    ['Standby']='xset dpms force standby'
    ['Hibernate']='systemctl hibernate'
  )

  local -r IFS=$'\n'
  # with some arguments:  execute a command mapped to $1
  # without any arguments show keys
  [[ $# -ne 0 ]] && eval "${menu[$1]}" || echo "${!menu[*]}"
}

main $@
