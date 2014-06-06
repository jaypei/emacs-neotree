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


Usage
-----

Buffer shortcuts

* `n` - next line
* `p` - previous line
* `SPC` or `RET` or `TAB` - open file / toggle expand folder
* `g` - refresh tree
* `C-c C-n` - create file or directory
* `C-c C-d` - delete file or directory
* `C-c C-c` - change root directory

Commands

* `M-x neotree-dir RET`
* `M-x neotree-show RET` or `M-x neotree RET`
* `M-x neotree-hide RET`
* `M-x neotree-toggle`


ChangeLog
---------

### 0.1.5 (2014-06-05)

* Refactory on code of window and buffer
* Remove \*NeoTree\* from buffer selection list
* Fix: Directory changed to root after toggle

### 0.1.4 (2014-05-27)

* Fix issue #1 #2
* Add some unit tests
* Remove neotree-util.el

### 0.1.3 (2014-04-13)

* Support for changing the directory of tree root
* Adding files or directories depends on if the last character is a '/'
* Remove 'Recursive Notification' when deleting an empty directory

### 0.1.2 (2014-04-08)

* Add File Operations
* Support For Mouse Operations

### 0.1.1 (2014-04-06)

Basic Functionalities.
