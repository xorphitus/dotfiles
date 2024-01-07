#!/usr/bin/env bash
spotifyd_stat() {
  meta=$(playerctl metadata -p spotifyd)
  title=$(echo "$meta" | grep title | awk '{print $3}')
  artist=$(echo "$meta" | grep artist | awk '{print $3}')
  echo "${title} / ${artist}" | sed 's/  */ /g'
}

ncspot_stat() {
  sock="$HOME/.cache/ncspot/ncspot.sock"
  if [ -f "$sock" ]; then
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

# Disabled spotifyd support: The status should be displayed in another machine
# via Spotify Connect
#
# if pgrep -x spotifyd > /dev/null; then
#   spotifyd_stat
# elif pgrep -x ncspot > /dev/null; then
#   ncspot_stat
# fi

if pgrep -x ncspot > /dev/null; then
  ncspot_stat
fi
