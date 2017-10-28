#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# start fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
fcitx &

# apply .Xmodmap
xmodmap ~/.Xmodmap

# set background image
#feh --bg-scale /usr/share/slim/themes/default/background.jpg

# set appearance (wallpaper, transparency)
nitrogen --restore &
compton &

# startup apps
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 8 --height 24 --transparent true --tint 0x000000 --alpha 15 &
terminator -mb -e tmux &
# replace to ~/.config/dunst/dunstrc by using /usr/share/dunst/dunstrc
dunst -lb 333333 -nb 000000 -hb 990000 &
dropbox &
skype &
# https://github.com/linuxmint/blueberry/blob/master/usr/lib/blueberry/blueberry.py#L30
XDG_CURRENT_DESKTOP=LXDE blueberry-tray &

# start window manager
exec xmonad

# turn off screen saver
#xset s off
