#!/bin/zsh
typeset -U progs
for prog in ${HOME}/.xmonad/AutoStart/*(*|@); do
    $prog &
done
