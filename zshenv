#
# Defines environment variables.
#
# Authors:
#   Tim Millican <tim.millican@linux.com>
#

# Ensure that a non-login, non-interactive shell has a defined environment. {{{
if [[ "$SHLVL" -eq 1 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi
# }}}
# Read local configuration {{{
if [[ -f ${ZDOTDIR:-$HOME}/.zshenv.local ]]; then
    source ${ZDOTDIR:-$HOME}/.zshenv.local
fi
# }}}

# vim: fdm=marker syntax=zsh
