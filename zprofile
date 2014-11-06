#
# Defines environment variables.
#
# Authors:
#   Tim Millican <tim.millican@linux.com>
#

# Editors {{{
if (( $+commands[vim] )); then
        export EDITOR=${commands[vim]}
        export VISUAL=${commands[vim]}
fi
# }}}
# Language {{{
if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi
# }}}
# Paths {{{
typeset -gU cdpath fpath mailpath path

# Set the the list of directories that cd searches.
cdpath=(
   $HOME
   $cdpath
)
# Remove non-existent directories
cdpath=($^cdpath(N-/))

# Set the list of directories that Zsh searches for programs.
path=(
  ${HOME}/bin
  ${HOME}/.local/bin
  ${HOME}/.cabal/bin
  $path
  /usr/local/{bin,sbin}
  /usr/{bin,sbin}
  /{bin,sbin}
  /opt/eclipse
  ${HOME}/.local/android-sdks/platform-tools
)
# Remove non-existent directories
path=($^path(N-/))
# }}}
# Less {{{
# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
if (( $+commands[lesspipe.sh] )); then
  export LESSOPEN='| /usr/bin/env lesspipe.sh %s 2>&-'
fi

PAGER="$commands[less] $LESS"
# }}}
# Temporary Files {{{
if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$USER"
  mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi
# }}}
# SSH & GPG Agents {{{
if [[ -s ${XDG_RUNTIME_DIR}/.gpg-agent.env ]]; then
    source ${XDG_RUNTIME_DIR}/.gpg-agent.env
fi
if [[ -S $XDG_RUNTIME_DIR/ssh-agent.sock ]]; then
    SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.sock
    export SSH_AUTH_SOCK
fi

# Inform gpg-agent of the current TTY for user prompts.
export GPG_TTY="$(tty)"
# }}}
# Extra {{{
export NNTPSERVER="nntp.aioe.org"
export EMERGE_DEFAULT_OPTS="--ask"
# }}}
# Read local configuration {{{
if [[ -f ${ZDOTDIR:-$HOME}/.zprofile.local ]]; then
    source ${ZDOTDIR:-$HOME}/.zprofile.local
fi
# }}}

# vim: fdm=marker syntax=zsh