#!/usr/bin/env bash
DATA=$(nc -W 1 -U ~/.cache/ncspot/ncspot.sock)

title=$(echo "$DATA" | jq -r '.playable.title')
artist=$(echo "$DATA" | jq -r '.playable.artists[0]')

stat="♪"
playing=$(echo "$DATA" | jq '.mode.Playing')
if [ "$playing" = "null" ]; then
  stat="⏸"
fi

echo "${stat}" "${title} / ${artist}" | sed 's/  */ /g'
