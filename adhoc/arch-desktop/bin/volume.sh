#!/bin/sh

sinks() {
  pacmd list-sinks \
    | grep 'index:' \
    | awk '{print $NF}'
}

ctl() {
  local percent=$1
  sinks | xargs -I {} bash -c "pactl set-sink-mute {} false && pactl set-sink-volume {} ${percent}"
}

toggle() {
  sinks | xargs -I {} pactl set-sink-mute {} toggle
  notify-send -u low '🔊'
}

down() {
  ctl '-5%'
  notify-send -u low '♪▼'
}

up() {
  ctl '+5%'
  notify-send -u low '♪▲'
}

status() {
  pactl list sinks | grep Volume | grep -v 'Base Volume' | awk '{print $5}' | sed -e 'N;s/\n/, /'
}

case $1 in
  "toggle")
    toggle;;
  "down")
    down;;
  "up")
    up;;
  "status")
    status;;
  *)
    exit 1
esac
