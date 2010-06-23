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
    Ryan-PC) #work-specific
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

    ;;
    progden) #dev server
	export EDITOR=/bin/local-emacs-edit.sh
	source /etc/profile.d/autojump.sh
	export CVS_RSH=ssh
	export CVSROOT=:ext:alb-desktop:/usr/local/cvsroot
    ;;
esac
