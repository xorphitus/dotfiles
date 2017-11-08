###########################################################
# general
set -x EDITOR emacsclient
set -x ALTERNATE_EDITOR vim
# set -x BROWSER google-chrome

###########################################################
# Emacs
set -x PATH $HOME/.cask/bin $PATH

###########################################################
# direnv
eval (direnv hook fish)

###########################################################
# fzf
set -x FZF_TMUX 1
set -x FZF_TMUX_HEIGHT 50%

###########################################################
# GNU Global (Gtags)
switch (uname)
    case Linux
        set -x GTAGSCONF /usr/share/gtags/gtags.conf
    case Darwin
        # undefined
end
# require python pygments
set -x GTAGSLABEL pygments

###########################################################
# Git
switch (uname)
    case Darwin
        set -x PATH /usr/local/share/git-core/contrib/diff-highlight $PATH
end

###########################################################
# Erlang
set -x ERLANG_HOME /usr/lib/erlang

###########################################################
# Scala
set -x ENSIME_ROOT $HOME/lib/aemoncannon-ensime-38627ca/src/main/

###########################################################
# Ruby
set RUBY_VERSION 2.4.1
rbenv global $RUBY_VERSION

###########################################################
# Node.js
set -x NODE_VERSION v4.3.2

###########################################################
# Go

###########################################################
# Common Lisp
alias sbcl='rlwrap sbcl'

###########################################################
# less
set -x PAGER 'less'
set -x LESS '-iMR --LONG-PROMPT'

set hiliter (which src-hilite-lesspipe.sh)
set -x LESSOPEN "| $hiliter %s"

###########################################################
# aliases
# alias google-chrome='google-chrome-stable -allow-file-access-from-files'
alias ack='ag'
alias l='ls'
alias e='emacsclient'
alias be='bundle exec'

###########################################################
# functions
function psg
    ps u | head -n 1
    set arg $argv[1]
    ps aux | grep $arg | grep -v "grep $arg"
end

function update-home-bin
    fisher up
    _install_nvm

    set targets '.rbenv' 'opt/clojurescript'
    for target in $targets
        cd $HOME/$target
        if test -d .git
            echo update $target
            git pull
        else
            echo $target is not version controled
        end
    end
end

# Notify when a command is finished
# usage:
#  $ some_long_command; n
function n
    # ntfy command might be better
    # https://github.com/dschep/ntfy
    switch (uname)
        case Linux
            notify-send -u low -t 3 "Command Finished!"
        case Darwin
            terminal-notifier -message "Command Finished!"
    end
end

##################
# fzf functions
function fzf_yaourt
    yaourt -Ss --color $argv | awk 'NR%2!=0' | sort | sed '1d' | fzf -m --ansi | cut -d" " -f1
end

function fzf_ps
    ps u | head -n 1
    set arg $argv[1]
    psg $arg | fzf +m --reverse
end

##################
# setup functions
function _install_nvm
    curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.2/install.sh | bash
end

function _setup_fishenv
    set packages 'fzf' 'direnv' 'rbenv' 'ruby-build' 'source-highlight' 'ghq' 'go' 'rlwrap' 'sbcl' 'ctags' 'global'
    switch (uname)
        case Linux
            yaourt -S    $packages python-pygments
        case Darwin
            brew install $packages terminal-notifier
    end

    # emacs cask
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

    # nvm
    _install_nvm

    # fisherman
    curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs https://git.io/fisher

    # plugins
    fisher z fzf decors/fish-ghq rbenv nvm

    # theme
    fisher install omf/theme-eden

    # programing languates
    rbenv install $RUBY_VERSION
    nvm install $NODE_VERSION
end
