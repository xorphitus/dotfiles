#!/bin/sh
readonly opcity=0.75

abort() {
  notify-send -u critical "error: you have to install package \`$1\`"
  exit 0
}

type xwininfo    > /dev/null || abort 'xorg-xwininfo'
type transset-df > /dev/null || abort 'transset-df'

xwininfo -all -root | fgrep 'i3bar for output' | xargs -I {} transset-df $opcity -i {} > /dev/null
