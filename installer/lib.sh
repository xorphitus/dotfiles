#!/bin/bash

if [ "$(uname)" == 'Darwin' ]; then
    OS='Mac'
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    OS='Linux'
else
    echo "($(uname -a)) is not supported."
    exit 1
fi

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
