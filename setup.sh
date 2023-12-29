#!/bin/bash

set -eux

DIR=$(cd $(dirname "$0"); pwd)

mkdir -p ~/.lein
files=( .emacs.d .lein/profiles.clj .muttrc .vimrc .config/fish/config.fish .config/wezterm )
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
