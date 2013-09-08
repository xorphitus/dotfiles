#!/bin/sh
DIR=`dirname $0`
cd $DIR
for f in $(find . -maxdepth 1 -name '.?*'); do
    FILE=`echo $f | sed -e "s/\.\///"`
    if [ $FILE == '.git' -o $FILE == '.gitignore' ]; then
        continue
    fi
    ln -s $DIR/$FILE $HOME/$FILE
done
