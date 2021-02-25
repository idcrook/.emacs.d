`site-lisp`
============

For gathering external Emacs lisp packages, especially if they fall outside normal package manager use. This is a place for 3rd party code which isn't available in MELPA or
other package repositories. This directory and its immediate subdirectories
will be added to load-path at start-up time.


<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

-	[Manually downloaded library](#manually-downloaded-library)
	-	[verilog-mode](#verilog-mode)
	-	[csh-mode](#csh-mode)
-	[Github](#github)
	-	[sysinfo](#sysinfo)
-	[Hacks](#hacks)

<!-- markdown-toc end -->

Manually downloaded library
===========================

Download each into its own (sub-)directory.

Add to script `./recompile.sh` (and run to update compiled version if needed)

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

Hacks
=====

Use `hacks` subdirectory to save one-time or one-off or experimental scripts.
