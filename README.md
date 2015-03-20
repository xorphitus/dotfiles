xorphitus dotfiles

## setup

execute setup.sh

## require

https://github.com/xorphitus/package-installer

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
