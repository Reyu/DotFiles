#!/bin/zsh
if [[ $(mpc | wc -l) > 1 ]]; then
    mpc toggle
else
    ~/.local/ZFunctions/roku play
fi
