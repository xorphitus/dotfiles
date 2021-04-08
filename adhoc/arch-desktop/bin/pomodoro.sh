#!/bin/sh
none() {
  echo '🍅 none'
  exit 0
}

pgrep emacs > /dev/null || none

stat=$(emacsclient --eval 'org-pomodoro-state' | sed -e 's/:pomodoro/🍅/' | sed -e 's/:.*-break/☕/')

[[ $stat = ":none" ]] && none

time=$(emacsclient --eval '(org-pomodoro-format-seconds)' | sed -e 's/"//g')

echo "$stat$time"
