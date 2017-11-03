#!/bin/zsh
source ${HOME}/.config/MyConfig
if [[ ${USE_PROMPT} == "powerline" ]]
then
    tmux source ${HOME}/.config/Tmux/plugins/airline/Airline
fi
