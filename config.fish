###########################################################
# general
set -x EDITOR emacsclient
set -x ALTERNATE_EDITOR vim
# set -x BROWSER google-chrome

###########################################################
# Emacs
set -x PATH $HOME/.cask/bin $PATH
alias e='emacsclient'

###########################################################
# direnv
eval (direnv hook fish)

###########################################################
# Erlang
set -x ERLANG_HOME /usr/lib/erlang

###########################################################
# Scala
set -x ENSIME_ROOT $HOME/lib/aemoncannon-ensime-38627ca/src/main/

###########################################################
# Ruby
set RUBY_VERSION 2.3.1
rbenv global $RUBY_VERSION

###########################################################
# Node.js
set -x NODE_VERSION v4.3.2

###########################################################
# Go
set -x GOPATH $HOME/go
set -x PATH $GOPATH/bin $PATH

###########################################################
# less
set -x PAGER 'less'
set -x LESS '-iMR --LONG-PROMPT'

## source-hightlight
#  yaourt -S souce-highlight
set hiliter (which src-hilite-lesspipe.sh)
set -x LESSOPEN "| $hiliter %s"

###########################################################
# aliases
# alias google-chrome='google-chrome-stable -allow-file-access-from-files'
alias ack='ag'

###########################################################
# functions
function psg
    ps u | head -n 1
    set arg $argv[1]
    ps aux | grep $arg | grep -v "grep $arg"
end

function _install_nvm
    curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.2/install.sh | bash
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

function _setup_fishenv
    # OS packages
    yaourt -S fzf direnv rbenv ruby-build

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
