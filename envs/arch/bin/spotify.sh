#!/usr/bin/env bash
spotifyd_stat() {
  meta=$(playerctl metadata -f '{{status}} {{title}} / {{artist}}' -p spotifyd)
  echo "$meta" | sed 's/  */ /g' | sed 's/Playing/♪/g' | sed 's/Paused/⏸/g' | sed 's/Stopped/⏹/g'
}

spotifyd_play_pause() {
  playerctl play-pause -p spotifyd
}

spotifyd_next() {
  playerctl next -p spotifyd
}

spotifyd_previous() {
  playerctl previous -p spotifyd
}

ncspot_stat() {
  sock="$HOME/.cache/ncspot/ncspot.sock"
  if [ -e "$sock" ]; then
    DATA=$(nc -W 1 -U "$sock")

    title=$(echo "$DATA" | jq -r '.playable.title')
    artist=$(echo "$DATA" | jq -r '.playable.artists[0]')

    stat="♪"
    playing=$(echo "$DATA" | jq '.mode.Playing')
    if [ "$playing" = "null" ]; then
      stat="⏸"
    fi

    echo "${stat}" "${title} / ${artist}" | sed 's/  */ /g'
  else
    echo "${sock} does not exist"
  fi
}

ncspot_play_pause() {
  "echo playpause | nc -W 1 -U ~/.cache/ncspot/ncspot.sock"
}

ncspot_next() {
  "echo next      | nc -W 1 -U ~/.cache/ncspot/ncspot.sock"
}

ncspot_previous() {
  "echo previous  | nc -W 1 -U ~/.cache/ncspot/ncspot.sock"
}

command=$1

if pgrep -x spotifyd > /dev/null; then
  case "$command" in
    stat)
      spotifyd_stat
      ;;
    play_pause)
      spotifyd_play_pause
      ;;
    next)
      spotifyd_next
      ;;
    previous)
      spotifyd_previous
      ;;
    *)
      spotifyd_stat
      ;;
  esac
elif pgrep -x ncspot > /dev/null; then
   case "$1" in
    stat)
      ncspot_stat
      ;;
    play_pause)
      ncspot_play_pause
      ;;
    next)
      ncspot_next
      ;;
    previous)
      ncspot_previous
      ;;
    *)
      ncspot_stat
      ;;
  esac
fi
