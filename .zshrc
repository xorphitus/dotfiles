###########################################################
# antigen
source ~/antigen/antigen.zsh

antigen use oh-my-zsh

# common plugins
antigen bundle bundler
antigen bundle git

# yaourt zsh-completions-git
antigen bundle zsh-users/zsh-completions
# yaourt zsh-syntax-highlighting-git
antigen bundle zsh-users/zsh-syntax-highlighting

# for each platforms
case ${OSTYPE} {
  linux*)
  antigen bundle archlinux
  ;;
  darwin*)
  antigen bundle brew
  antigen bundle brew-cask
  ;;
}

antigen-theme juanghurtado

## prezto
# antigen use prezto
# antigen bundle sorin-ionescu/prezto
# antigen-theme pure

# Tell antigen that you're done.
antigen apply

###########################################################
# general
export EDITOR=emacsclient
export ALTERNATE_EDITOR=vim
export BROWSER=google-chrome

###########################################################
# emacs
export PATH="$HOME/.cask/bin:$PATH"
alias e='emacsclient'

case ${OSTYPE} {
  darwin*)
  alias emacs='exec /usr/local/Cellar/emacs/24.4/Emacs.app/Contents/MacOS/Emacs "$@"'
  ;;
}

###########################################################
# erlang
export ERLANG_HOME=/usr/lib/erlang

###########################################################
# scala
export ENSIME_ROOT=${HOME}/lib/aemoncannon-ensime-38627ca/src/main/

###########################################################
# anyenv
export PATH="$HOME/.anyenv/bin:$PATH"
eval "$(anyenv init -)"

###########################################################
# python
PYTHON_VERSION="3.3.4"
pyenv global $PYTHON_VERSION

export PATH=${PYENV_ROOT}/shims:$PATH

###########################################################
# php
phpenv global 5.5.10

###########################################################
# ruby
RUBY_VERSION=2.2.2
rbenv global $RUBY_VERSION

export PATH=${RBENV_ROOT}/shims:$PATH

###########################################################
# nodejs
NODE_VERSION="v0.10"
ndenv global $NODE_VERSION

NODE_ROOT=${NDENV_ROOT}/versions/${NODE_VERSION}
export NODE_PATH=${NODE_ROOT}/lib/node_modules
export PATH=${NODE_ROOT}/bin:$PATH

###########################################################
# ClojureScript
export CLOJURESCRIPT_HOME=~/opt/clojurescript

function cljs-repljs() {
    ${CLOJURESCRIPT_HOME}/script/repljs $@
}

function cljsc() {
    ${CLOJURESCRIPT_HOME}/bin/cljsc $1 '{:optimizations :advanced}'
}

###########################################################
# Go lang
export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin

###########################################################
# less
export PAGER='less'
export LESS='-iMR --LONG-PROMPT'

## source-hightlight
#  yaourt -S souce-highlight
export LESSOPEN="| `which src-hilite-lesspipe.sh` %s"

###########################################################
# peco

# select command from history
function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | \
                    sort | \
                    uniq -c | \
                    sort | \
                    awk '{$1=""; print}' | \
                    eval $tac | \
                    peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-select-history
bindkey '^r' peco-select-history

# kill proccess from ps
function peco-proc-kill () {
    ps ax -o pid,lstart,command | peco --query "$LBUFFER" | awk '{print $1}' | xargs kill
    zle clear-screen
}
zle -N peco-proc-kill
bindkey '^x^p' peco-proc-kill

###########################################################
# aliases
alias google-chrome='google-chrome -allow-file-access-from-files'
alias ack='ag'

###########################################################
# functions
function google() {
    local str opt
    if [ $# != 0 ]; then
        for i in $*; do
            str="$str+$i"
        done
        str=`echo $str | sed 's/^\+//'`
        opt='search?num=50&hl=ja&ie=euc-jp&oe=euc-jp&lr=lang_ja'
        opt="${opt}&q=${str}"
    fi
    w3m http://www.google.co.jp/$opt
}

function psg() {
    ps u | head -n 1
    ps aux | grep $1 | grep -v "grep $1"
}

function update-home-bin() {
    anyenv update
    antigen update

    targets=('opt/clojurescript')
    for target in $targets;
    do
        cd ~/${target}
        if [ -d .git ]; then
            echo update $target
            git pull
        else
            echo $target is not version controled
        fi
    done
}
