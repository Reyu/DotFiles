#!/bin/zsh
for plug in $(print ${0:A:h}/*/*.tmux(.)); do
    tmux run-shell $plug
done
