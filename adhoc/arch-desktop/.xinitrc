#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# start ibus
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="ibus"
export XMODIFIERS=@im=ibus
ibus-daemon --xim -d &

# apply .Xmodmap
xmodmap ~/.Xmodmap

# set background image
#feh --bg-scale /usr/share/slim/themes/default/background.jpg 

# set appearance (wallpaper, transparency)
nitrogen --restore &
compton &

# startup apps
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 13 --height 17 --transparent true --tint 0x000000 --alpha 10 &
terminator -mb -e tmux &
dunst &
dropbox &
skype &

# start window manager
exec xmonad

# turn off screen saver
#xset s off
