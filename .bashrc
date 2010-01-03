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

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

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

case $(hostname) in
    ryan-laptop) #eeepc-specific
	__ensure_agent
	export EDITOR=emacsclient
	export EMAIL="ryan@mokeys.org"
	;;
    ryan) #work-specific
	__ensure_agent
	export EDITOR=emacsclient	
	export GIT_SSH=ssh
	export SVN_SSH=ssh
	export PATH="/usr/local/bin:/usr/bin:/bin:/usr/X11R6/bin:$PATH"
	
	export HISTSIZE=1000  #double default
    #append our current history to the file, clear the hist, then load from file.
	export PROMPT_COMMAND="history -a;history -c; history -r;$PROMPT_COMMAND"
	export HISTIGNORE="history *:cd *:df *:exit:fg:bg:file *:clear"
    ;;
    sakimet) #dev server
	export EDITOR=nano
	source /etc/profile.d/autojump.sh
	export CVS_RSH=ssh
	export CVSROOT=:ext:alb-desktop:/usr/local/cvsroot
	export PS1='\[\e]0;\u@\h: \w\a\]\n\[\e[32m\]\u@\h: \[\e[33m\]\w\[\e[1;34m\]$(__git_ps1)\[\e[0m\]\n > '
    ;;
esac

#various settings that depend on the machine-specific stuff
export DARCS_EMAIL="Ryan Davis <${EMAIL}>"

echo "Checking for config updates... "
config pull

