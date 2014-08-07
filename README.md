emacs-neotree
=============

A emacs tree plugin like NerdTree for Vim.

`Develop` [![Build Status](https://travis-ci.org/jaypei/emacs-neotree.svg?branch=dev)](https://travis-ci.org/jaypei/emacs-neotree)
`Master` [![Build Status](https://travis-ci.org/jaypei/emacs-neotree.svg?branch=master)](https://travis-ci.org/jaypei/emacs-neotree)


Screenshots
-----------
![NeoTree-1] (https://raw.githubusercontent.com/wiki/jaypei/emacs-neotree/imgs/neotree-1.png)

Installation
------------

Clone project:
```sh
$ cd /some/path
$ git clone https://github.com/jaypei/emacs-neotree.git neotree
```

Add config to emacs:

```elisp
(add-to-list 'load-path "/some/path/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
```

Open (toggle) NeoTree:

```
<F8>
```

Usage:

* `n` - next line
* `p` - previous line
* `SPC` or `RET` or `TAB` - open file / toggle expand folder

More documentation:

* [EmacsWiki](http://www.emacswiki.org/emacs/NeoTree)
* [中文版 NeoTree](http://www.emacswiki.org/emacs-zh/NeoTree_%E4%B8%AD%E6%96%87wiki)

