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
tint2 &
terminator -mb -e tmux &
dunst &
dropboxd &
skype &

# start window manager
exec openbox-session

# turn off screen saver
#xset s off
