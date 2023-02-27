#!/bin/sh
if lsusb | grep -i -e webcam -e camera > /dev/null; then
  echo '🎥 ON'
else
  echo ''
fi
