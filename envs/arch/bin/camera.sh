#!/bin/sh
if lsusb | grep -i webcam > /dev/null; then
  echo '🎥 ON'
else
  echo ''
fi
