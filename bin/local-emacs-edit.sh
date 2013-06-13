#!/bin/sh
if [ -n $ALTERNATE_EDITOR ]; then
    ALTERNATE_EDITOR=nano
fi

if ! which emacsclient; then 
    echo "no emacsclient"
    $ALTERNATE_EDITOR $1
    exit
fi

if ! [ -r ~/.emacs.d/server/server ]; then
    echo "No server info file."
    $ALTERNATE_EDITOR $1
    exit
fi

#build the tramp filename
FN="/`whoami`@`hostname -f`:"
case $1 in
     /*) FN="$FN$1" ;;
     *) FN="$FN`pwd`/$1" ;;
esac

echo "$FN"
#we point it at the file written by the ssh wrapper for port and auth
#information
emacsclient -a "$ALTERNATE_EDITOR" -f ~/.emacs.d/server/server "$FN"
