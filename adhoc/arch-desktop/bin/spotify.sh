#!/bin/sh
DATA=$(playerctl --player=spotify metadata)

extract() {
  data="$1"
  attr="$2"
  echo "$data" | grep "$attr" | awk '{c="";for(i=3;i<=NF;i++) c=c $i" "; print c}'
}


title=$(extract "$DATA" 'xesam:title')
artist=$(extract "$DATA" 'xesam:albumArtist')

echo "${title} / ${artist}" | sed 's/  */ /g'
