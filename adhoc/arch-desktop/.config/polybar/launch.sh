#!/bin/sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u "$UID" -x polybar >/dev/null; do sleep 1; done

MONITORS=$(i3-msg -t get_outputs | jq -r '.[] | select(.active==true) | .name')

MONITORS=$MONITORS polybar main &
MONITORS=$MONITORS polybar sub &
