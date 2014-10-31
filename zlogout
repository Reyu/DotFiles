#
# Executes commands at logout.
#
# Authors:
#   Tim Millican <tim.millican@linux.com>
#

# Print the message.
if [[ $USER != 'root' ]]; then
cat <<-EOF
The world is coming to an end--save your buffers!
EOF
fi

if [[ -f ${ZDOTDIR:-$HOME}/.zlogout.local ]]; then
    source ${ZDOTDIR:-$HOME}/.zlogout.local
fi
