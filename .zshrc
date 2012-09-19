export BROWSER=google-chrome
export FILEMANAGER=pcmanfm
export ERLANG_HOME=/usr/lib/erlang
export ENSIME_ROOT=$HOME/lib/aemoncannon-ensime-38627ca/src/main/
export GEM_HOME=~/.gem/ruby/1.9.1/

export PATH=${PATH}:${GEM_HOME}/bin

fpath=($HOME/.zsh $fpath)

bindkey -e

autoload -Uz colors
colors
setopt prompt_subst

setopt auto_cd

# enable back-space-key
bindkey '^@' backward-delete-char

###########################################################
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
autoload -U compinit
compinit

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z} r:|[-_.]=**'

zstyle ':completion:*:default' menu select=1

zstyle ':completion:*:(processes|jobs)' menu yes select=2

###########################################################

PROMPT='%n:%l@%m$ '
RPROMPT='%~'

###########################################################
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
# pythonbrew
# https://github.com/utahta/pythonbrew
[[ -s $HOME/.pythonbrew/etc/bashrc ]] && source $HOME/.pythonbrew/etc/bashrc

###########################################################
# rbenv
# https://github.com/sstephenson/rbenv/
eval "$(rbenv init -)"

###########################################################
# color settings

# grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='01;33'

# source-hightlight
#  yaourt -S souce-highlight
export LESS='-R'
export LESSOPEN='| /usr/bin/src-hilite-lesspipe.sh %s'

###########################################################
# ~/.bash_aliases, instead of adding them here directly.
#if [ -f ~/.aliases ]; then
#    . ~/.aliases
#fi
alias ls='ls -F --color=auto'
alias l='ls'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias w3mg='w3m http://www.google.co.jp/'
alias google-chrome='google-chrome -allow-file-access-from-files'
alias chrome='google-chrome'
alias pybrew='pythonbrew'

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

function _yaourt() {
    local install_flg=0
    if [ $# != 0 ]; then
	for i in $*; do
	    if [ $i = '-S' ]; then
		install_flg=1
		break
	    fi
	done
    fi
    yaourt $@

    if [ $install_flg -gt 0 ]; then
	rehash
    fi
}
alias yaourt='_yaourt'

function _pacman() {
    local install_flg=0
    if [ $# != 0 ]; then
	for i in $*; do
	    if [ $i = '-S' ]; then
		install_flg=1
		break
	    fi
	done
    fi
    pacman $@

    if [ $install_flg -gt 0 ]; then
	rehash
    fi
}
alias pacman='_pacman'

function psg() {
    ps u | head -n 1
    ps aux | grep $1 | grep -v "grep $1"
}

function wcon() {
    PID_FILE="/run/dhcpd-wlan0.pid"
    if [ -d ${PID_FILE} ]; then
        sudo kill -9 `cat ${PID_FILE}`
    fi

    #iwconfig wlan0 mode ad-hoc
    sudo ifconfig wlan0 up

    # without encription
    # iwconfig wlan0 essid {ESSID}

    # WEP
    # iwconfig wlan0 essid {ESSID} key {KEY}

    # WPA/WPA2
    #echo ctrl_interface=DIR=/var/run/wpa_wpa_supplicant GROUP=wheel > /etc/wpa_supplicant.conf
    #wpa_passphrase {wireless_ssid} {secretpassphrase} >> /etc/wpa_supplicant.conf
    sudo wpa_supplicant -B -Dwext -i wlan0 -c /etc/wpa_supplicant.conf

    sudo dhcpcd wlan0
}
