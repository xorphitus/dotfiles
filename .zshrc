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
# erlang
export ERLANG_HOME=/usr/lib/erlang

###########################################################
# scala
export ENSIME_ROOT=${HOME}/lib/aemoncannon-ensime-38627ca/src/main/

###########################################################
# python

## pyenv

# see .zshenv


###########################################################
# ruby
alias be='bundle exec'

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

rbenv global 2.0.0-p247

###########################################################
# nodejs
[[ -s ~/.nvm/nvm.sh ]] && . ~/.nvm/nvm.sh
nvm use 0.11.7
export NODE_PATH=${NVM_PATH}_modules

###########################################################
# php

# phpenv
export PATH="$HOME/.phpenv/bin:$PATH"
eval "$(phpenv init -)"

phpenv global 5.5.3

###########################################################
# less
export PAGER='less'
export LESS='-R --LONG-PROMPT'

## source-hightlight
#  yaourt -S souce-highlight
export LESSOPEN="| `which src-hilite-lesspipe.sh` %s"

###########################################################
# grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='01;33'

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

###########################################################
# for each platforms
case ${OSTYPE} {
    linux*)
        . ${HOME}/.zshrc.linux;;
    darwin*)
        . ${HOME}/.zshrc.mac;;
}

###########################################################
# prompt
setopt prompt_subst

PROMPT="%{${fg[yellow]}%}[%~]%{${reset_color}%}
%n@%m$ "

RPROMPT="%1(v|%F{green}%1v%f|)${vcs_info_git_pushed}"

