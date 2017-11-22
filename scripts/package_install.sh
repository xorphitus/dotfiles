# FIXME

#!/bin/bash

if [ "$(uname)" == 'Darwin' ]; then
    OS='Mac'
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    OS='Linux'
else
    echo "($(uname -a)) is not supported."
    exit 1
fi

if [ $OS == 'Mac' ]; then
    if type brew > /dev/null 2>&1; then
        echo 'brew is found'
        exit 0
    else
        echo 'Error: You have to install homebrew!'
        exit 1
    fi
else
    if type yaourt > /dev/null 2>&1; then
        echo 'yaourt is found'
    else
        echo 'Error: You have to install yaourt!'
        exit 1
    fi
f

function pacman() {
    pacman -Q $1 > /dev/null 2>&1
    if [ $? -eq 1 ]; then
        pacman -S $1
    fi
}

function aur() {
    pacman -Q $1 > /dev/null 2>&1
    if [ $? -eq 1 ]; then
        yaourt -S $1
    fi
}

function brew() {
    local path="/usr/local/Cellar/$1"
    if [ -d $path ]; then
        echo $1 is alreadly installed
    else
        brew install $@
    fi
}

function install() {
    if [ $OS == 'Mac' ]; then
        brew $@
    else
        pacman $@
    fi
}

### enable gcc and other cli tools
if [ $OS == 'Mac' ]; then
    xcode-select --install
    install iproute2mac
    install telnet
fi

### shell
install fish
which fish >> /etc/shells
chsh -s $(which fish)

### tmux
install tmux

### git
install git

### font
if [ $OS == 'Mac' ]; then
    install ricty --with-powerline
else
    aur ttf-ricty
    install ttf-symbola
fi

### find -> fd
if [ $OS == 'Mac' ]; then
    install fd
else
    install fd-rs
fi

### grep -> ag
install the_silver_searcher

### top -> htop
install htop

### vim
install vim

### json
install jq

### stats
install iotop

if [ $OS == 'Linux' ]; then
    install dstat
fi

### tree
install tree

### emacs
if [ $OS == 'Mac' ]; then
    install emacs --with-cocoa --srgb
    sudo easy_install pip
    sudo pip install pygments
    install global --with-exuberant-ctags --with-pygments
    install cmigemo
else
    install emacs
    aur global
    aur cmigemo-git
fi

readonly SKKDICT_PATH=~/.local/share/skk
mkdir -p $SKKDICT_PATH
cd $SKKDICT_PATH
curl -O http://openlab.jp/skk/dic/SKK-JISYO.L.gz
curl -O http://openlab.jp/skk/dic/SKK-JISYO.geo.gz
curl -O http://openlab.jp/skk/dic/SKK-JISYO.jinmei.gz
curl -O http://openlab.jp/skk/dic/SKK-JISYO.propernoun.gz
curl -O http://openlab.jp/skk/dic/SKK-JISYO.station.gz
gunzip *
cd -

### programing languages
install leiningen
install haskell-stack
install erlang

### other basic tools
if [ $OS == 'Linux' ]; then
    install openssl
    install alsa-utils
    install lsof
    install unzip
    # media ripper
    install abcde
    aur dropbox-cli

    ## GUI tools
    install ibus-mozc
    install xorg-xmodmap
    install terminator
    install gpicview # image viewer
    install libreoffice-base
    install libreoffice-calc
    install libreoffice-common
    install libreoffice-draw
    install libreoffice-gnome
    install libreoffice-impress
    install libreoffice-math
    install libreoffice-writer
    install libreoffice-ja
    install gimp
    install skype
    install flashplugin
    install firefox-i18n-ja
    aur google-chrome
    install dunst
    install compton
    install nitrogen

    ## xmonad
    install xmonad
    install xmonad-contrib
    install xmobar

    # middleware
    install virtualbox
    install vagrant
    install docker
    # for Wine
    aur lib32-mesa-full-i965
else
    echo "###############"
    echo "install GUI tools shown below"
    echo "  * Google Chrome"
    echo "  * Google Japanese Input"
    echo "  * Slack Client"
    echo "  * Docker for Mac"
    echo "  * MacPass"
    echo "  * Karabiner"
fi
