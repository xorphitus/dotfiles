#!/bin/bash

set -eux

../../setup.sh

DIR=$(cd $(dirname "$0"); pwd)

files=( .xprofile .config/dunst .config/i3 .config/polybar .config/picom.conf .config/rofi )
for i in "${files[@]}"
do
  src="$DIR/$i"
  if [ -e "$src" ]; then
    ln -sf "$src" "$HOME/$i"
  else
    echo "$src doesn't exist"
    exit 1
  fi
done
