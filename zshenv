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
# Make sure that SSH and GPG agents are accessible {{{
if [[ -f ${HOME}/.ssh-agent.env ]]; then
    source ${HOME}/.ssh-agent.env &> /dev/null
    if ! ssh-add -l &> /dev/null; then
        eval $(ssh-agent | tee ${HOME}/.ssh-agent.env)
    fi
else
    eval $(ssh-agent | tee ${HOME}/.ssh-agent.env)
fi
if [[ -z $GPG_AGENT_INFO ]]; then
    if [[ -f ${HOME}/.gpg-agent.env ]]; then
        source ${HOME}/.gpg-agent.env
    else
        eval $(gpg-agent --daemon --write-env-file "${HOME}/.gpg-agent.env")
    fi
fi
# }}}
# EC2 Tools {{{
if [[ -f $HOME/.aws_keys ]]; then
    export AWS_ACCESS_KEY=$(cat $HOME/.aws_keys|head -n1)
    export AWS_SECRET_KEY=$(cat $HOME/.aws_keys|tail -n1)
fi
if [[ -d $HOME/.local/ec2-tools ]];then
    export EC2_HOME=$HOME/.local/ec2-tools
    export JAVA_HOME=$(print -l /usr/lib/jvm/java-*-openjdk-*/jre|sort|tail -n1)
    export PATH=$PATH:$EC2_HOME/bin
fi
# }}}
# Read local configuration {{{
if [[ -f ${ZDOTDIR:-$HOME}/.zshenv.local ]]; then
    source ${ZDOTDIR:-$HOME}/.zshenv.local
fi
# }}}

# vim: fdm=marker syntax=zsh
