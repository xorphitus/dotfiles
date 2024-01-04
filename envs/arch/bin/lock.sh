#!/usr/bin/env bash

B='#00000099'  # blank
C='#00000099'  # clear ish
D='#00ffffcc'  # default
T='#00eeeeee'  # text
W='#00008855'  # wrong
V='#00bbbb55'  # verifying

export LC_ALL=C

i3lock \
--insidever-color=$C   \
--ringver-color=$V     \
\
--insidewrong-color=$C \
--ringwrong-color=$W   \
\
--inside-color=$B      \
--ring-color=$D        \
--line-color=$B        \
--separator-color=$D   \
\
--verif-color=$T        \
--wrong-color=$T        \
--time-color=$T        \
--date-color=$T        \
--layout-color=$T      \
--keyhl-color=$W       \
--bshl-color=$W        \
\
--screen 1            \
--blur 16              \
--clock               \
--indicator           \
--time-str="%H:%M:%S"  \
--date-str="%A, %m %Y" \
--keylayout 2
