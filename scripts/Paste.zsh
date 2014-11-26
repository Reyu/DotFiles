#!/bin/zsh 
TEXT=$(`which xclip` -selection ${1:-primary} -out)

if [[ ! -z $TEXT ]]; then
    exec `which xdotool` type "$TEXT"
fi
