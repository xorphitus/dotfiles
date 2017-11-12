# FIXME

#!/bin/bash

. ./lib.sh

if [ $OS == 'Mac' ]; then
    which brew
    echo 'install homebrew!'
    exit 1
else
    which yaourt
    echo 'install yaourt!'
    exit 1
fi

### enable gcc and other cli tools
if [ $OS == 'Mac' ]; then
    xcode-select --install
    install iproute2mac
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
    install ricty
else
    install ttf-ricty
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
fi

if [ $OS == 'Mac' ]; then
    echo "###############"
    echo "install GUI tools shown below"
    echo "  * Google Chrome"
    echo "  * Google Japanese Input"
    echo "  * Slack Client"
    echo "  * Docker for Mac"
    echo "  * MacPass"
    echo "  * Karabiner"
fi
