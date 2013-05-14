#!/bin/bash

rdesktop -u ryepup -d acceleration -a 16 -g 1600x1024 -x b -r disk:home=/home/ryan/ -r clipboard:CLIPBOARD ${1-10.201.35.213}

