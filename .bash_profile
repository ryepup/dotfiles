# base-files version 3.7-1

# To pick up the latest recommended .bash_profile content,
# look in /etc/defaults/etc/skel/.bash_profile

# Modifying /etc/skel/.bash_profile directly will prevent
# setup from updating it.

# The copy in your home directory (~/.bash_profile) is yours, please
# feel free to customise it to create a shell
# environment to your liking.  If you feel a change
# would be benifitial to all, please feel free to send
# a patch to the cygwin mailing list.

# ~/.bash_profile: executed by bash for login shells.

# source the system wide bashrc if it exists
if [ -e /etc/bash.bashrc ] ; then
  source /etc/bash.bashrc
fi

# source the users bashrc if it exists
if [ -e "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi

# Set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/bin" ] ; then
   PATH=${HOME}/bin:${PATH}
fi

# Set MANPATH so it includes users' private man if it exists
# if [ -d "${HOME}/man" ]; then
#   MANPATH=${HOME}/man:${MANPATH}
# fi

# Set INFOPATH so it includes users' private info if it exists
# if [ -d "${HOME}/info" ]; then
#   INFOPATH=${HOME}/info:${INFOPATH}
# fi

alias config="git --git-dir=${HOME}/.config.git/ --work-tree=${HOME}"
alias ll='ls -hal'

export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'

case $(hostname) in
    ryan)
    
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
	
	
	function kill-agent {
	    pid=`cat /tmp/.ssh-agent-pid`
	    kill $pid
	}
	
	export GIT_SSH=ssh
	export SVN_SSH=ssh
	
	export PATH="/usr/local/bin:/usr/bin:/bin:/usr/X11R6/bin:$PATH"
	
	export EDITOR=emacsclient
	
	shopt -s histappend
    #ignore lines that match previous entry or start with a space.
	export HISTCONTROL=ignoreboth
	export HISTSIZE=1000  #double default
    #append our current history to the file, clear the hist, then load from file.
	export PROMPT_COMMAND="history -a;history -c; history -r;$PROMPT_COMMAND"
	export HISTIGNORE="history *:cd *:df *:exit:fg:bg:file *:clear"

    ;;
    sakimet)
	source /etc/profile.d/autojump.sh
	export CVS_RSH=ssh
	export CVSROOT=:ext:alb-desktop:/usr/local/cvsroot
	export PS1='\[\e]0;\u@\h: \w\a\]\n\[\e[32m\]\u@\h: \[\e[33m\]\w\[\e[1;34m\]$(__git_ps1)\[\e[0m\]\n > '
    ;;
    *)
esac


export DARCS_EMAIL="Ryan Davis <ryan@acceleration.net>"
export EMAIL="ryan@acceleration.net"
export PYTHONSTARTUP=~/.pythonrc

echo "Checking for config updates... "
config pull
