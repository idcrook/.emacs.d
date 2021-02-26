`site-lisp`
===========

For gathering external Emacs lisp packages, especially if they fall outside normal package manager use. This is a place for 3rd party code which isn't available in MELPA or other package repositories. This directory and its immediate subdirectories will be added to load-path at start-up time.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Manually downloaded libraries](#manually-downloaded-libraries)
    - [csh-mode](#csh-mode)
- [Hacks](#hacks)
- [Outdated](#outdated)
    - [verilog-mode](#verilog-mode)
    - [github](#github)

<!-- markdown-toc end -->

Manually downloaded libraries
=============================

Download each into its own (sub-)directory.

Add to script `./recompile.sh` (and run to update compiled version if needed)

csh-mode
--------

```
curl https://opensource.apple.com/source/tcsh/tcsh-67/tcsh/csh-mode.el > csh-mode/csh-mode.el
```

Hacks
=====

Use `hacks` subdirectory to save one-time or one-off or experimental scripts.

Outdated
========

verilog-mode
------------

Now in ELPA. No longer manually downleaded. See https://github.com/veripool/verilog-mode/blob/master/README.adoc#INSTALLATION

github
------

For this type of module, now instead of downloading or adding a git submodule, use `straight.el`: [raxod502/straight.el: 🍀 Next-generation, purely functional package manager for the Emacs hacker.](https://github.com/raxod502/straight.el)
