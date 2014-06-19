###########################################################
# general
export EDITOR=emacsclient
export ALTERNATE_EDITOR=vim
export BROWSER=google-chrome

fpath=(${HOME}/.zsh ${fpath})

bindkey -e

autoload -Uz colors
colors

autoload -Uz add-zsh-hook

setopt auto_cd

# enable back-space-key
bindkey '^@' backward-delete-char

setopt no_beep
setopt no_list_beep
setopt auto_list
setopt list_types
#setopt always_to_end
#setopt recexact

###########################################################
# for each platforms
case ${OSTYPE} {
    linux*)
        . ${HOME}/.zshrc.linux;;
    darwin*)
        . ${HOME}/.zshrc.mac;;
}

###########################################################
# completion
autoload -U compinit
compinit

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*:(processes|jobs)' menu yes select=2

# color
# set LS_COLORS
eval `dircolors -b`
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

## option
#unsetopt bash_auto_list
unsetopt list_ambiguous
unsetopt menu_complete
setopt auto_menu
setopt always_last_prompt
setopt complete_in_word
setopt auto_remove_slash
setopt extended_glob
setopt auto_param_keys
setopt magic_equal_subst

###########################################################
# history
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
export HISTFILE=${HOME}/.zsh_history
export HISTSIZE=1000
export SAVEHIST=500
setopt extended_history
setopt share_history

setopt auto_pushd

setopt pushd_ignore_dups


###########################################################
# vcs_info
#  http://mollifier.hatenablog.com/entry/20100906/p1
autoload -Uz vcs_info

zstyle ':vcs_info:*' enable git svn hg bzr
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
zstyle ':vcs_info:bzr:*' use-simple true
 
autoload -Uz is-at-least
if is-at-least 4.3.10; then
  zstyle ':vcs_info:git:*' check-for-changes true
  zstyle ':vcs_info:git:*' stagedstr "+"
  zstyle ':vcs_info:git:*' unstagedstr "-"
  zstyle ':vcs_info:git:*' formats '(%s)-[%b] %c%u'
  zstyle ':vcs_info:git:*' actionformats '(%s)-[%b|%a] %c%u'
fi
 
function _update_vcs_info_msg() {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    psvar[2]=$(_git_not_pushed)
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}

function _git_not_pushed() {
    if [ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" = "true" ]; then
        head="$(git rev-parse HEAD)"
        for x in $(git rev-parse --remotes)
        do
            if [ "$head" = "$x" ]; then
                return 0
            fi
        done
        echo "|?"
    fi
    #return 0
    echo ok
}

add-zsh-hook precmd _update_vcs_info_msg

###########################################################
# emacs

# cask
export PATH="${HOME}/.cask/bin:$PATH"

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
alias be='bundle exec'

RUBY_VERSION=2.1.2
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
# less
export PAGER='less'
export LESS='-iMR --LONG-PROMPT'

## source-hightlight
#  yaourt -S souce-highlight
export LESSOPEN="| `which src-hilite-lesspipe.sh` %s"

###########################################################
# grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='01;33'

###########################################################
# zaw
source ~/opt/zaw/zaw.zsh
bindkey '^r' zaw-history
zstyle ':filter-select' max-lines $(($LINES / 2))

###########################################################
# aliases
alias ls='ls -F --color=auto'
alias l='ls'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias w3mg='w3m http://www.google.co.jp/'
alias google-chrome='google-chrome -allow-file-access-from-files'
alias e='emacsclient -n'
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
    targets=('opt/zaw' 'opt/clojurescript')
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

###########################################################
# prompt
setopt prompt_subst

PROMPT="%{${fg[yellow]}%}[%~]%{${reset_color}%}
%n@%m$ "

RPROMPT="%1(v|%F{green}%1v%f|)${vcs_info_git_pushed}"
