emacs-neotree
=============

A emacs tree plugin like NerdTree.

Screenshots
-----------
![NeoTree-1] (https://raw.githubusercontent.com/wiki/jaypei/emacs-neotree/imgs/neotree-1.png)

Installation
------------

Clone project
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
* `C-x C-f` or `C-c C-f` - find or create file
* `C-c C-d` - delete file or directory


ChangeLog
---------

### 0.1.1 (2014-04-06)

基本功能完成

### 0.1.2 (2014-04-08)

增加基本文件操作，支持鼠标选中

