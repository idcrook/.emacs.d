#!/bin/bash
#!/bin/bash -x

# need to explicitly add new .el files to bytecomp.el

if [[ x`uname -s` == x"Linux" ]] ; then
    Emacs=emacs
else
    #Emacs=/opt/homebrew/opt/emacs-plus/Emacs.app/Contents/MacOS/Emacs
    Emacs=/Applications/Emacs.app/Contents/MacOS/Emacs
fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

for subdir in csh-mode ; do

    echo Compiling "${subdir}"
    cd "$DIR"/"${subdir}" || (echo error; exit 1)

    # compile these .el files as standalone elisp
    $Emacs --batch --no-init-file --no-site-file --directory ~/.emacs.d/site-lisp/"${subdir}"/ -f batch-byte-compile *.el

done
