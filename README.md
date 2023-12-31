xorphitus dotfiles

## Setup

execute `envs/${my_env}/setup.sh` or `setup.sh`.

## Require

https://github.com/xorphitus/package-installer

### For Emacs
Rust components

* rls
* rust-src
* rust-analysis
* clippy
* rustfm

OS packages

* rust-analyzer
* cmigemo
* multimarkdown
* rg
* PlantUML

and execute the followings

* `M-x all-the-icons-install-fonts`
* `M-x nerd-icons-install-fonts`

For AI packages:

* OS package installation
  * ollama
* Install AI models for Olamma and run `ollama serve`
* Set `openai-key` to Pass

### gitconfig

Create `~/home/.gitconfig` file as following.

```
[include]
	path = /path/to/dotfiles/.gitconfig
[sendemail]
	smtpserver     = example.com
	smtpserverport = 587
	smtpencryption = tls
	smtpuser       = xorphitus@example.com
[credential]
	helper = cache
[core]
	autocrlf = input
[init]
	defaultBranch = main
```

### git / diff-highlight

#### Arch Linux

```
$ ln -s /usr/share/git/diff-highlight /hoge/diff-highlight
```

`/hoge` = `/usr/local/bin` ?

#### OSX

```
$ ln -s /usr/local/Cellar/git/**/contrib/diff-highlight/diff-highlight /hoge/diff-highlight
```
