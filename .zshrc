###########################################################
# general
export BROWSER=google-chrome

fpath=(${HOME}/.zsh ${fpath})

bindkey -e

autoload -Uz colors
colors
setopt prompt_subst

setopt auto_cd

# enable back-space-key
bindkey '^@' backward-delete-char

setopt no_beep
setopt no_list_beep
setopt auto_list
#unsetopt bash_auto_list
unsetopt list_ambiguous
unsetopt menu_complete
setopt auto_menu
setopt always_last_prompt
setopt complete_in_word
#setopt always_to_end
setopt list_types
#setopt recexact
setopt auto_remove_slash
setopt auto_param_keys
setopt extended_glob

###########################################################
# completion
autoload -U compinit
compinit

# color
eval `dircolors -b`
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z} r:|[-_.]=**'
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*:(processes|jobs)' menu yes select=2

###########################################################
# prompt
PROMPT='%n:%l@%m$ '
RPROMPT='%~'

###########################################################
# history
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=1000
export SAVEHIST=500
setopt extended_history
setopt share_history

setopt auto_pushd

setopt pushd_ignore_dups

###########################################################
# erlang
export ERLANG_HOME=/usr/lib/erlang

###########################################################
# scala
export ENSIME_ROOT=${HOME}/lib/aemoncannon-ensime-38627ca/src/main/

###########################################################
# python

## pythonbrew
# https://github.com/utahta/pythonbrew
[[ -s ${HOME}/.pythonbrew/etc/bashrc ]] && source ${HOME}/.pythonbrew/etc/bashrc

###########################################################
# ruby
export GEM_HOME=~/.gem/ruby/1.9.1/
export PATH=${PATH}:${GEM_HOME}/bin

# rbenv
# https://github.com/sstephenson/rbenv/
eval "$(rbenv init -)"

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
alias pybrew='pythonbrew'

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
case $OSTYPE {
    linux*)
        . ${HOME}/.zshrc.linux;;
    darwin*)
        . ${HOME}/.zshrc.mac;;
}
