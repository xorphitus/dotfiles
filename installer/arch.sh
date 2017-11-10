#!/bin/bash
#
# 昔作ったものをそのまま移植
# たぶん見直しが必要

function install() {
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

# basic tools
install emacs
install zsh # TODO change to fish
install tmux
install openssl
install git
install vim
install alsa-utils
install mercurial

# CUI tools
install tree
install tig
install the_silver_searcher
install lsof
install source-highlight
install unzip
install htop
install dstat
install iotop
install abcde # media ripper

aur jq
aur dropbox-cli
aur emacs-ddskk
aur peco-git
aur cmigemo-git

# GUI tools
install xorg-xmodmap
install terminator
install gpicview
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
install vlc
install hardinfo
install skype
install flashplugin
install firefox-i18n-ja
install dunst
install nitrogen

aur xmonad
aur xmonad-contrib
aur xmobar
aur ttf-ricty
aur compton
aur nitrogen
aur google-chrome

# Lang
install scala
install gauche
install clisp
install erlang

aur lighttable-git

aur leiningen
aur haskell-platform
# for PHP
aur bison27

# middleware
install virtualbox
install vagrant
install docker
# install mariadb
# install postgresql
# install mongodb
# install elasticsearch

# for Wine
aur lib32-mesa-full-i965
