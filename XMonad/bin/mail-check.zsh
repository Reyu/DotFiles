#!/bin/zsh
typeset -a MAILBOXES
MAILBOXES=(
    'Inbox'
    'Lists.Gentoo.User:Gentoo User'
    'Lists.Vim:VIM'
    'Lists.ZFS:ZFS'
)

HAS_MAIL=0
DETAIL=""
for box in $MAILBOXES
do
    box=("${(s/:/)box}")
    loc=$box[1]
    name=$box[2]

    count=$(print -ln ${HOME}/Mail/${loc}/new/*(.N) | wc -l)
    if [[ $count > 0 ]]
    then
        HAS_MAIL=1
    fi

    DETAIL="${DETAIL}${name:-$box}: ${count}\n"
done

if [[ $HAS_MAIL == 1 ]]
then
    TITLE="New Mail!"
else
    TITLE=""
fi

print "^tw() ${TITLE}\n^cs()\n${DETAIL}"
