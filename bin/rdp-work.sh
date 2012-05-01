#!/bin/bash

rdesktop -u ryepup -d acceleration -a 16 -g 1280x960 -x b -r disk:home=/home/ryan/ ${1-10.201.35.213}
