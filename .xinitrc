#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# start scim
export XMODIFIERS=@im=SCIM
export GTK_IM_MODULE="scim"
export QT_IM_MODULE="scim"
scim -d

# start uim
#export GTK_IM_MODULE='uim'
#export QT_IM_MODULE='uim'
#uim-xim &
#export XMODIFIERS=@im='uim'

# start ibus
#export GTK_IM_MODULE="ibus"
#export QT_IM_MODULE="ibus"
#export XMODIFIERS=@im=ibus
#ibus-daemon --xim -d &

# apply .Xmodmap
xmodmap ~/.Xmodmap

# set background image
feh --bg-scale /usr/share/slim/themes/default/background.jpg 

# start tint2
tint2 &

# startups 
conky &
urxvt -e tmux &

# set transparency
#xcompmgr &

# start window manager
exec openbox

# turn off screen saver
xset s off
