#!/bin/sh

del_old() {
  dir=$1
  find "$dir" -type f -atime +30 -print0 | xargs rm
  find "$dir" -type d -empty     -print0 | xargs rm -r
}

del_old ~/junk
del_old ~/tmp
del_old ~/Downloads
