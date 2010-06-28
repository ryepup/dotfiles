#!/bin/sh

if [ -n $ALTERNATE_EDITOR ]; then
    ALTERNATE_EDITOR=nano
fi

if ! which emacsclient; then 
    echo "no emacsclient"
    $ALTERNATE_EDITOR $1
    exit $?
fi

if ! [ -r ~/.emacs.d/server/server ]; then
    echo "No server info file."
    $ALTERNATE_EDITOR $1
    exit $?
fi
    
PORT=`egrep  -o '127.0.0.1:([0-9]*)' ~/.emacs.d/server/server | sed 's/127.0.0.1://'`
if ! nc -z 127.0.0.1 $PORT; then  #anyone listening on that port?
    echo "Port back to emacs server not open: $PORT"
    $ALTERNATE_EDITOR $1
    exit $?
fi;    

#build the tramp filename
FN="/`whoami`@`hostname`:`ls -1 $(pwd)/$1`"
echo "$FN"
#we point it at the file written by the ssh wrapper for port and auth
#information
emacsclient -f ~/.emacs.d/server/server "$FN"
