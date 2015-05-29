#!/bin/zsh
sleep 2s
typeset -U progs
for prog in ${HOME}/.xmonad/AutoStart/*(*|@); do
    $prog &
    sleep .2s
done
