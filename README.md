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

Add emacs config:

```elisp
(add-to-list 'load-path "/some/path/neotree")
(require 'neotree)
```

Open NeoTree:

```
M-x neotree RET
```

Usage
-----

* `n` - next line
* `p` - previous line
* `SPC` or `RET` or `TAB` - open file / toggle expand folder
* `g` - refresh tree
* `C-c C-n` - create file or directory
* `C-c C-d` - delete file or directory
* `C-c C-c` - change root directory


ChangeLog
---------

### 0.1.1 (2014-04-06)

基本功能完成

### 0.1.2 (2014-04-08)

增加基本文件操作，支持鼠标选中

### 0.1.3 (2014-04-13)

支持改变根节点目录，增加目录和文件根据末尾/判断，删除空目录时去掉递归提醒

