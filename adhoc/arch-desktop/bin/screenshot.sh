#!/bin/sh

abort() {
  echo $1 >&2
  exit 0
}

type import > /dev/null || abort 'no `import` command found. you may need to install `imagemagic` package.'

import -window root ~/screenshot-$(date "+%Y%m%d-%H%M%S%N").jpg
