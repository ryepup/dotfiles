# -*- mode: sh *-*
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
