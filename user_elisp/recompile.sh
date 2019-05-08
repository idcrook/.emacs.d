#!/bin/bash -x

# need to explicitly add new .el files to bytecomp.el

if [[ x`uname -s` == x"Linux" ]] ; then
   Emacs=emacs
else
   Emacs=/Applications/Emacs.app/Contents/MacOS/Emacs
fi

# compile these .el files as standalone elisp
$Emacs --batch --no-init-file  --no-site-file --directory ~/.emacs.d/user_elisp/ -f batch-byte-compile *.el
