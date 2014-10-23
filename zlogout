#
# Executes commands at logout.
#
# Authors:
#   Tim Millican <tim.millican@linux.com>
#

# Print the message.
cat <<-EOF
The world is coming to an end--save your buffers!
EOF

if [[ -f ${ZDOTDIR:-$HOME}/.zlogout.local ]]; then
    source ${ZDOTDIR:-$HOME}/.zlogout.local
fi
