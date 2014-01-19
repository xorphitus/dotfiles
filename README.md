xorphitus dotfiles

## setup

execute setup.sh

### require

#### Ruby

* rbenv
* gems
  * rubocop
  * rcodetools
  * sass
  * slim
  * haml (optional)

#### Node

* nvm
* npm
  * jshint
  * coffee-script
  * coffeelint
  * csslint
  * less
  * jsonlint
  * swank-js

#### Clojure

leiningen

#### Python (optional)

* pyenv
* pip
  * pylint
  * pep8
  * flake8
  * pyflakes

#### etc

* zaw


### git / diff-highlight

#### Arch Linux

```
ln -s /usr/share/git/diff-highlight /hoge/diff-highlight
```

`/hoge` = `/usr/local/bin` ?

#### OSX

```
ln -s /usr/local/Cellar/git/**/contrib/diff-highlight/diff-highlight /hoge/diff-highlight
```

### swank-js

Slime for JavaScript

```
$ npm install -g swank-js
$ ln -s /path/to/node_modules/swank-js/slime-js.el /path/to/elpa/slime-*/contrib
```

### rcodetools

Ruby development tools

```
$ gem install rcodetools
$ rbenv rehash
$ ln -s /path/to/gems/rcodetools-${VERSION}/rcodetools.el /path/to/elisp/
```
