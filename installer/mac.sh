#!/bin/bash
#
# まったく動作確認していないんだが
# セットアップ作業を下手にDocに残すよりはスクリプト化しておいた方が分かりやすいかなと思って記述

readonly PACKAGE_ROOT=/usr/local/Cellar

function install() {
    local path="${PACKAGE_ROOT}/$1"
    if [ -d $path ]; then
        echo $1 is alreadly installed
    else
        brew install $@
    fi
}

install fish
install tmux
install ricty
install git
install jq
install htop
install iproute2mac
install tree
install leiningen
install haskell-stack
install erlang

# emacs and dependencies
install emacs --with-cocoa --srgb
install global --with-exuberant-ctags --with-pygments
install ag
install cmigemo

readonly SKKDICT_PATH=~/skk
mkdir -p $SKKDICT_PATH
cd $SKKDICT_PATH
curl -O http://openlab.jp/skk/dic/SKK-JISYO.L.gz
curl -O http://openlab.jp/skk/dic/SKK-JISYO.geo.gz
curl -O http://openlab.jp/skk/dic/SKK-JISYO.jinmei.gz
curl -O http://openlab.jp/skk/dic/SKK-JISYO.propernoun.gz
curl -O http://openlab.jp/skk/dic/SKK-JISYO.station.gz
gunzip *
cd -

# enable gcc and other cli tools
xcode-select --install

# manual istallation guide
echo "###############"
echo "set fish as default shell"
echo "  add /usr/local/bin/fish to /etc/shells"
echo "  and execute chsh -s /usr/local/bin/fish"
echo ""

echo "###############"
echo "exetute _setup_fishenv (my fish function)"
echo "  it installes additional packages which my fish config depends on"
echo ""

echo "###############"
echo "install GUI tools shown below"
echo "  * Google Chrome"
echo "  * Google Japanese Input"
echo "  * MacPass"
echo "  * Karabiner"
echo "  * Slack Client"
echo "  * Docker for Mac"
