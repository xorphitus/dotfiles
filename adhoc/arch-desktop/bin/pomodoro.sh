#!/bin/sh
none() {
  echo '🍅 none'
  exit 0
}

pgrep emacs > /dev/null || none

estat=$(emacsclient --eval 'org-pomodoro-state' 2>&1)

echo "$estat" | grep ERROR > /dev/null && none
stat=$(echo "$estat" | sed -e 's/:pomodoro/🍅/' | sed -e 's/:.*-break/☕/')

[[ "$stat" = ":none" ]] && none

etime=$(emacsclient --eval '(org-pomodoro-format-seconds)' 2>&1)
echo "$etime" | grep ERROR > /dev/null && none
time=$(echo "$etime" | sed -e 's/"//g')

echo "$stat$time"
