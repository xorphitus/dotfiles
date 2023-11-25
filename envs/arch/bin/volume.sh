#!/usr/bin/env bash

# JBL Pebbles cannot make sound when the volume is less than 87%, and somehow
# 87% seems not be able to let the both left and right speakers work appropriately.
# Therefore, 88% may be the best as the minimum volume.
MIN_VOLUME_PERCENT=88

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
  notify-send -u low ' / 🔇'
}

# Parse the following format output and obtain only the first volume with the % unit.
# In the following example, `88` percent of the `front-left` volume will be plucked.
#
# Volume: front-left: 57434 /  88% / -3.44 dB,   front-right: 57428 /  88% / -3.44 dB
#         balance -0.00
get_current_volume_percent() {
  index=$1
  pactl get-sink-volume "$index" | head -n 1 | cut -d '/' -f 2 | sed -e 's/ *//g' | sed -e 's/%//'
}

ensure_min() {
  for sink in $(sinks); do
    vol=$(get_current_volume_percent "$sink")

    if [[ "$vol" =~ ^[0-9]+$ ]] && [ "$vol" -lt "$MIN_VOLUME_PERCENT" ]; then
      ctl "${MIN_VOLUME_PERCENT}%"
    fi
  done
}

down() {
  ctl '-5%'
  ensure_min
  notify-send -u low '🔉' # '🔈'
}

up() {
  ctl '+5%'
  notify-send -u low '🔊'
}

volumes() {
  pactl list sinks | grep Volume | grep -v 'Base Volume' | awk '{print $5}'
}

status() {
  volumes | tr '\n' ' '
}

graphical_status() {
  local ave=$(volumes | sed -e 's/%//g' | awk '{m+=$1} END{print m/NR;}')
  # max 10 steps
  local degree=$(echo $ave | awk '{print int($1 / 20)}')
  local bar=""
  for i in $(seq 1 10) ; do
    if [ $i -le $degree ]; then
      bar=$(echo $bar '➤')
    else
      bar=$(echo $bar '・')
    fi
  done
  local icon=''
  pactl list sinks | fgrep 'Mute: yes' > /dev/null && icon='🔇'
  echo "$icon [$bar] $(status)"
}

case $1 in
  "toggle")
    toggle;;
  "down")
    down;;
  "up")
    up;;
  "status")
    graphical_status;;
  *)
    exit 1
esac
