#!/bin/zsh
sleep 2s
typeset -U progs
for prog in ${HOME}/.xmonad/AutoStart/*(*|@); do
    $prog >! ${HOME}/.xmonad/logs/${prog:t}.log 2>&1 &
    sleep .2s
done
