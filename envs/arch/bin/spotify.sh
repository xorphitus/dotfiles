#!/bin/sh
DATA=$(nc -W 1 -U ~/.cache/ncspot/ncspot.sock)

title=$(echo "$DATA" | jq -r '.playable.title')
artist=$(echo "$DATA" | jq -r '.playable.artists[0]')

echo "${title} / ${artist}" | sed 's/  */ /g'
