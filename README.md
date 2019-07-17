xorphitus dotfiles

## setup

execute setup.sh

## require

https://github.com/xorphitus/package-installer

### for Emacs
some Rust components

* rls
* clippy
* rustfmt
* rust-src

other CLI tools

* ag
* multimarkdown

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

### rcodetools

Ruby development tools

```
$ gem install rcodetools
$ rbenv rehash
$ ln -s /path/to/gems/rcodetools-${VERSION}/rcodetools.el /path/to/elisp/
```
