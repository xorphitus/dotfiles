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
  notify-send -u low ' / 🔇'
}

down() {
  ctl '-5%'
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
  volumes | sed -e 'N;s/\n/, /'
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
  echo $icon '[' $bar ']' $(status)
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
