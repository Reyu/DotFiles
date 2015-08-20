#
# Executes commands at login post-zshrc.
#
# Authors:
#   Tim Millican <tim.millican@linux.com>
#

# Execute code that does not affect the current session in the background. {{{
{
  # Compile the completion dump to increase startup speed.
  zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
  if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
    zcompile "$zcompdump"
  fi
} &!
# }}}
# Read local configuration {{{
if [[ -f ${ZDOTDIR:-$HOME}/.zlogin.local ]]; then
    source ${ZDOTDIR:-$HOME}/.zlogin.local
fi
# }}}

# vim: fdm=marker syntax=zsh
