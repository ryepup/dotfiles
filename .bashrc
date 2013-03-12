# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

#global options
alias config="git --git-dir=${HOME}/.config.git/ --work-tree=${HOME}"
alias ll='ls -hal'
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'
export EMAIL="ryan@acceleration.net"
export PYTHONSTARTUP=~/.pythonrc
export PS1='\[\e]0;\u@\h: \w\a\]\n\[\e[32m\]\u@\h: \[\e[33m\]\w\[\e[1;34m\]$(__git_ps1)\[\e[0m\]\n$ '


function kill-agent {
    pid=`cat /tmp/.ssh-agent-pid`
    kill $pid
}

function __ensure_agent {
    export SSH_AUTH_SOCK='/tmp/.ssh-socket'
    ssh-add -l 2>&1 >/dev/null
    if [ $? = 2 ]; then
# Exit status 2 means couldn't connect to ssh-agent; start one now
	rm $SSH_AUTH_SOCK
	ssh-agent -a $SSH_AUTH_SOCK >/tmp/.ssh-script
	. /tmp/.ssh-script
	echo $SSH_AGENT_PID >/tmp/.ssh-agent-pid
	ssh-add
    fi
}

function svn_line_changes {
    echo "Scanning $@"
    removed=`svn diff $@ | grep "^-[^-]" | wc -l`
    echo "Removed: $removed"
    added=`svn diff $@ | grep "^+[^+]" | wc -l`
    echo "Added: $added"
    difference=`expr $added - $removed`
    echo "Difference: $difference"
    unset removed added difference
}

. local.bashrc

#various settings that depend on the machine-specific stuff
export DARCS_EMAIL="Ryan Davis <${EMAIL}>"
git config --global user.email $EMAIL

function git_repeat {
    date
    while ! git $1 ; do sleep 1; date; done
}
