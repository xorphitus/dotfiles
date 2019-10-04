#!/bin/sh
abort() {
  echo "error: you have to install package \`$1\`" >&2
  exit 1
}

set -eu
: $1 $2

readonly target=$1
readonly opcity=$2

type xwininfo    > /dev/null || abort 'xorg-xwininfo'
type transset-df > /dev/null || abort 'transset-df'

readonly targets=$(xwininfo -all -root | grep $target)

echo target windows
echo '--------'
echo $targets

echo $targets | xargs -I {} transset-df $opcity -i {} > /dev/null

