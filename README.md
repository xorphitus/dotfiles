xorphitus dotfiles

## setup

execute setup.sh

## require

https://github.com/xorphitus/package-installer

### for Emacs
some Rust components

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

and execute `M-x all-the-icons-install-fonts`

### git / global ignore

see: https://qiita.com/ueokande/items/e0409219e7c68e4277b9

```
$ ln -s ~/dotfiles/.gitignore_global  ~/.config/git/ignore
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
