# vim: fdm=marker syntax=zsh

# Load extras
source ${zdir}/lib/git.zsh
source ${zdir}/lib/darcs.zsh

# Write some info to the terminal title
function precmd {
    if [[ -z $EMACS ]]
    then
        print -Pn "\e]2;%~%(1j, - %j job%(2j|s|),)\a"
    fi
}
# Write command and args to terminal title
# This is soon while the shell waits for a command to complete
function preexec {
    # Use command name only, or if this is a sudo or ssh call,
    # then use the next command
    if [[ -z $EMACS ]]
    then
        print -Pn "\e]2;${1[(wr)^(*=*|sudo|ssh|-*)]:gs/%/%%:g}\a"
    fi
}

# Variable prompt character, based on repo type 
function prompt_char {
    if (( $+commands[git] )) && git branch &> /dev/null
    then
        echo '±'
    else
        echo '>'
    fi
}

# System hostname, displayed in alternate color if remote
function prompt_host {
    if [[ -z $SSH_CONNECTION ]]
    then
        # print -n '%{%F{136}%}%m%{%f%}'
        print -n '%F{136}%m%f'
    else
        # print -n '%{%F{166}%}%m%{%f%}'
        print -n '%F{166}%m%f'
    fi
}

# Display Python VirtualEnv, if set
function prompt_venv {
    if [[ -n $VIRTUAL_ENV ]]
    then
        # print -n "%{%F{33}%}(%{%F{64}%}${VIRTUAL_ENV:t}%{%F{33}%})%{%f%} "
        print -n "%F{33}(%F{64}${VIRTUAL_ENV:t}%F{33})%f "
    fi
}

function reset-prompt-and-accept-line {
    zle reset-prompt
    zle accept-line
}
zle -N reset-prompt-and-accept-line
bindkey '^m' reset-prompt-and-accept-line

# Prompt
PROMPT='
%(!,%F{160},%F{37})%n%f at $(prompt_host) in %F{64}%B%(5~,../%4c,%~)%b%f$(git_prompt)$(darcs_prompt)
[%*] $(prompt_venv)%(1j,J%j ,)$(prompt_char) '
