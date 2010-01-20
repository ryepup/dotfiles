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
export PS1='\[\e]0;\u@\h: \w\a\]\n\[\e[32m\]\u@\h: \[\e[33m\]\w\[\e[1;34m\]$(__git_ps1)\[\e[0m\]\n > '


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

case $(hostname) in
    ryan-laptop) #eeepc-specific
	__ensure_agent
	export EDITOR=emacsclient
	export EMAIL="ryan@mokeys.org"

	function rpd_stop-lapse(){
	    ps --pid `cat ~/cam/pid` > /dev/null
	    if [ $? = 0 ]; then
		kill `cat ~/cam/pid`
	    fi
	    rm ~/cam/pid ~/cam/log
	}
	function rpd_start_lapse(){
	    freq=$1
	    if [ -z "$freq" ]; then
		freq=30
	    fi
	    fswebcam --pid ~/cam/pid --log ~/cam/log -b -l $freq -r 352x288 --no-banner --png -1 ~/cam/%F-%T.png
	    echo "capture started, will take a shot every $freq seconds."
	    unset freq
	}

	function rpd_process_timelapse(){
	    i=0; 
	    imgPrefix=$1
	    for f in $@; 
	    do 
		if [ -z "$first" ]; then
		    first=yes
		else
		    cp $f $(printf ${imgPrefix}%05d.png $i); 
		    i=`expr $i + 1`; 
		fi
	    done;
	    echo "Copy/renamed $i images"
	    ffmpeg -r 10 -qscale 5 -i ${imgPrefix}%05d.png ${imgPrefix}.mp4
	    echo "wrote ${imgPrefix}.mp4"
	    rm ${imgPrefix}*.png
	    unset i imgPrefix first
	}
	echo "Checking for config updates... "
	config pull

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

	function offsite-backup {
	    time rsync -vrtz --delete --exclude=PendingDeletion/ /cygdrive/n/Static-Servers/ /cygdrive/f/Static-Servers
	}

	echo "Checking for config updates... "
	config pull

    ;;
    sakimet) #dev server
	export EDITOR=nano
	source /etc/profile.d/autojump.sh
	export CVS_RSH=ssh
	export CVSROOT=:ext:alb-desktop:/usr/local/cvsroot
    ;;
esac

#various settings that depend on the machine-specific stuff
export DARCS_EMAIL="Ryan Davis <${EMAIL}>"

