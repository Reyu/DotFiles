#!/bin/zsh
if tmux has -t $1
then
    exec tmux attach -t $1
else
    exec tmux new -s $1
fi
