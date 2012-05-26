#!/bin/sh
cd `dirname $0`
for f in $(find . -maxdepth 1 -name '.?*'); do
    ln -s $f ~
done
