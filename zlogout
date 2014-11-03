#
# Executes commands at logout.
#
# Authors:
#   Tim Millican <tim.millican@linux.com>
#

# Read local configuration {{{
if [[ -f ${ZDOTDIR:-$HOME}/.zlogout.local ]]; then
    source ${ZDOTDIR:-$HOME}/.zlogout.local
fi
# }}}

if [[ $USER != 'root' ]]; then
cat <<-EOF
The world is coming to an end--save your buffers!
EOF
fi

# vim: fdm=marker syntax=zsh
