`user_elisp`
============

For gathering other external Emacs lisp packages

Manually downloaded library
===========================

Download each into its own directory.

verilog-mode
------------

```
curl https://www.veripool.org/ftp/verilog-mode.el.gz | gzip -cd > verilog-mode/verilog-mode.el
```

csh-mode
--------

```
curl https://opensource.apple.com/source/tcsh/tcsh-67/tcsh/csh-mode.el > csh-mode/csh-mode.el
```

Github
======

If the emacs lisp library is available in a proper Git clone, fork it and point to that. Doing it this way (could also use `straight.el`) allows more conscious control of any customizations or updates.

Just clone as a `git submodule` a desired (even un-"packaged") emacs library into `github` subdirectory.

sysinfo
-------

```
(cd github && git submodule add https://github.com/idcrook/sysinfo.git)
```
