#!/bin/sh
readonly target=$1
readonly opcity=$2

abort() {
  echo "error: you have to install package \`$1\`" >&2
  exit 0
}

type xwininfo    > /dev/null || abort 'xorg-xwininfo'
type transset-df > /dev/null || abort 'transset-df'

readonly targets=$(xwininfo -all -root | grep $target)

echo target windows
echo '--------'
echo $targets

echo $targets | xargs -I {} transset-df $opcity -i {} > /dev/null
