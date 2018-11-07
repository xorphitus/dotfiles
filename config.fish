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
# SKK
switch (uname)
    case Linux
        set -x SKK_DICT_PATH /usr/share/skk
    case Darwin
        set -x SKK_DICT_PATH ~/skk
end

###########################################################
# Erlang
set -x ERLANG_HOME /usr/lib/erlang

###########################################################
# Scala
set -x ENSIME_ROOT $HOME/lib/aemoncannon-ensime-38627ca/src/main/

###########################################################
# Ruby
set RUBY_VERSION 2.5.1
rbenv global $RUBY_VERSION

###########################################################
# Node.js
set -x NODE_VERSION system

###########################################################
# Go

###########################################################
# Common Lisp
alias sbcl='rlwrap sbcl'

###########################################################
# Python
. (pyenv init - | psub)

###########################################################
# less
set -x PAGER 'less'
set -x LESS '-iMR --LONG-PROMPT'

set hiliter (which src-hilite-lesspipe.sh)
set -x LESSOPEN "| $hiliter %s"

###########################################################
# aliases
# alias google-chrome='google-chrome-stable -allow-file-access-from-files'
alias ls='exa'
alias ack='ag'
alias diff='colordiff'
alias l='ls'
alias be='bundle exec'
alias diff2='diff -ybBw'

###########################################################
# functions
function psg
    ps u | head -n 1
    set arg $argv[1]
    ps aux | grep $arg | grep -v "grep $arg"
end

function railsnew
    set prj $argv[1]
    mkdir $prj
    cd $prj
    bundle init
    echo "gem 'rails'" >> Gemfile
    bundle install --path vendor/bundle
    yes | bundle exec rails new .
    echo '/vendor/bundle' >> .gitignore
end

function update-home-bin
    fisher up
    _install_nvm

    set targets '.rbenv'
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

function update-skk-dict
    mkdir -p $SKK_DICT_PATH

    set tmppath /tmp/skk-dict
    mkdir -p $tmppath
    cd $tmppath

    wget http://openlab.jp/skk/dic/SKK-JISYO.L.gz
    wget http://openlab.jp/skk/dic/SKK-JISYO.geo.gz
    wget http://openlab.jp/skk/dic/SKK-JISYO.jinmei.gz
    wget http://openlab.jp/skk/dic/SKK-JISYO.propernoun.gz
    wget http://openlab.jp/skk/dic/SKK-JISYO.station.gz
    gunzip ./*.gz

    switch (uname)
        case Linux
            sudo mv ./SKK-JISYO* $SKK_DICT_PATH
        case Darwin
            mv ./SKK-JISYO* $SKK_DICT_PATH
    end

    cd -
    rm -rf $tmppath
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

# Extended EmacsClient
# * with arguments: normal emacsclient
# * without arguments: Piping stdout to an Emacs buffer using emacsclient
#   * e.g. `$ echo foo | e`
#
# https://www.emacswiki.org/emacs/EmacsClient#toc45
# http://d.hatena.ne.jp/kitokitoki/20111225/p4
function e
    switch (count $argv)
        case 0
            set tmp (mktemp /tmp/emacsstdinXXXXXX)
            set elisp "(let ((b (create-file-buffer \"*stdin*\"))) (switch-to-buffer b) (insert-file-contents \"$tmp\") (delete-file \"$tmp\"))"
            cat > $tmp
            if not emacsclient -a /usr/bin/false -e $elisp > /dev/null 2>&1
                emacs -e $elisp &
            end
        case '*'
            emacsclient -a emacs -n $argv > /dev/null 2>&1 &
    end
end

# dislplay shell buffer in Emacs
function es
    tmux capture-pane -S -10000\; show-buffer | e
end

# open new terminal window
# it should be used in Emacs M-!
# to associate Emacs with shell
function t
    switch (uname)
        case Linux
            terminator -x tmux
        case Darwin
            open -a Terminal (pwd)
    end
end

##################
# fzf functions
function fzf_yay
    yay -Ss --color $argv | awk 'NR%2!=0' | sort | sed '1d' | fzf -m --ansi | cut -d" " -f1
end

function fzf_ps
    ps u | head -n 1
    set arg $argv[1]
    psg $arg | fzf +m --reverse
end

function fzf_z
    set dir (z -l $argv | awk '{print $2}' | fzf +m --reverse)
    cd $dir
end

function fzf_ssh
    set server (grep 'Host ' ~/.ssh/config | fgrep -v '*' | awk '{print $2}' | sort | fzf +m --reverse)
    ssh $server
end

# fzf mosh targets
# the targets are found in ~/.ssh/config
# when `# mosh` comment is annotated
function fzf_mosh
    set server (fgrep '# mosh' -A 3 ~/.ssh/config | grep 'Host ' | awk '{print $2}' | sort | fzf +m --reverse)
    mosh $server
end

##################
# setup functions
function _install_nvm
    curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash
end

function _setup_fishenv
    set packages 'fzf' 'direnv' 'rbenv' 'ruby-build' 'source-highlight' 'ghq' 'go' 'rlwrap' 'sbcl' 'ctags' 'global' 'exa' 'colordiff'
    switch (uname)
        case Linux
            yay -S       $packages python-pygments
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
    fisher z fzf masa0x80/ghq_cd_keybind.fish rbenv nvm

    # theme
    fisher install omf/theme-eden

    # programing languates
    rbenv install $RUBY_VERSION
    nvm install $NODE_VERSION
end

set -x GITHUB_TOKEN 21bd1e2b837a1756b5ac7f89a3e2c8de2c2eaa94

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.fish ]; and . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.fish
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.fish ]; and . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.fish

# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/google-cloud-sdk/path.fish.inc" ]; . "$HOME/google-cloud-sdk/path.fish.inc"; end
