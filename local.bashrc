# -*- mode: sh; -*-
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
    time rsync -rtz --delete --exclude=PendingDeletion/ /cygdrive/n/Static-Servers/ /cygdrive/f/Static-Servers
    time rsync -rtz --delete /cygdrive/n/OfficeShare/khufu-OfficeShare/ /cygdrive/f/OfficeShare/khufu-OfficeShare
}

echo "Checking for config updates... "
config pull


