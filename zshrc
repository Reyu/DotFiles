#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Tim Millican <tim.millican@linux.com>
#

# Prepend Stuff {{{
# Where are we?
zdir=$(print ${ZDOTDIR:-$HOME}/.zshrc(:A:h))

# History Stuff
export HISTFILE=${HOME}/.zhistory
export HISTSIZE=10000
export SAVEHIST=10000

# Treat these characters as part of a word.
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

# Use human-friendly identifiers.
typeset -gA key_info
key_info=(
  'Control'   '\C-'
  'Escape'    '\e'
  'Meta'      '\M-'
  'Backspace' "^?"
  'Delete'    "^[[3~"
  'F1'        "$terminfo[kf1]"
  'F2'        "$terminfo[kf2]"
  'F3'        "$terminfo[kf3]"
  'F4'        "$terminfo[kf4]"
  'F5'        "$terminfo[kf5]"
  'F6'        "$terminfo[kf6]"
  'F7'        "$terminfo[kf7]"
  'F8'        "$terminfo[kf8]"
  'F9'        "$terminfo[kf9]"
  'F10'       "$terminfo[kf10]"
  'F11'       "$terminfo[kf11]"
  'F12'       "$terminfo[kf12]"
  'Insert'    "$terminfo[kich1]"
  'Home'      "$terminfo[khome]"
  'PageUp'    "$terminfo[kpp]"
  'End'       "$terminfo[kend]"
  'PageDown'  "$terminfo[knp]"
  'Up'        "$terminfo[kcuu1]"
  'Left'      "$terminfo[kcub1]"
  'Down'      "$terminfo[kcud1]"
  'Right'     "$terminfo[kcuf1]"
  'BackTab'   "$terminfo[kcbt]"
)

# Set empty $key_info values to an invalid UTF-8 sequence to induce silent
# bindkey failure.
for key in "${(k)key_info[@]}"; do
  if [[ -z "$key_info[$key]" ]]; then
    key_info["$key"]='�'
  fi
done
# }}}
# ZSH Options & Modules {{{
setopt ALWAYS_TO_END          # Move cursor to the end of a completed word.
setopt AUTO_CD                # Auto changes to a directory without typing cd.
setopt AUTO_LIST              # Automatically list choices on ambiguous completion.
setopt AUTO_MENU              # Show completion menu on a successive tab press.
setopt AUTO_NAME_DIRS         # Auto add variable-stored paths to ~ list.
setopt AUTO_PARAM_SLASH       # If completed parameter is a directory, add a trailing slash.
setopt AUTO_PUSHD             # Push the old directory onto the stack on cd.
setopt AUTO_RESUME            # Attempt to resume existing job before creating a new process.
setopt BANG_HIST              # Treat the '!' character specially during expansion.
setopt CDABLE_VARS            # Change directory to a path stored in a variable.
setopt COMPLETE_IN_WORD       # Complete from both ends of a word.
setopt CORRECT                # Turn on spell correcting
setopt CORRECT_ALL            # Spelling correction for arguments
setopt DVORAK                 # Use Dvorak keyboard for correcting mistakes
setopt EXTENDED_GLOB          # Use extended globbing syntax.
setopt EXTENDED_HISTORY       # Write the history file in the ':start:elapsed;command' format.
setopt HIST_BEEP              # Beep when accessing non-existent history.
setopt HIST_EXPIRE_DUPS_FIRST # Expire a duplicate event first when trimming history.
setopt HIST_FIND_NO_DUPS      # Do not display a previously found event.
setopt HIST_IGNORE_ALL_DUPS   # Delete an old recorded event if a new event is a duplicate.
setopt HIST_IGNORE_DUPS       # Do not record an event that was just recorded again.
setopt HIST_IGNORE_SPACE      # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS      # Do not write a duplicate event to the history file.
setopt HIST_VERIFY            # Do not execute immediately upon history expansion.
setopt INC_APPEND_HISTORY     # Write to the history file immediately, not when the shell exits.
setopt INTERACTIVE_COMMENTS   # Allow comments even in interactive shells
setopt LONG_LIST_JOBS         # List jobs in the long format by default.
setopt MULTIOS                # Write to multiple descriptors.
setopt NOTIFY                 # Report status of background jobs immediately.
setopt PATH_DIRS              # Perform path search even on command names with slashes.
setopt PUSHD_IGNORE_DUPS      # Do not store duplicates in the stack.
setopt PUSHD_SILENT           # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME          # Push to home directory when no argument is given.
setopt RC_QUOTES              # Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'.
setopt SHARE_HISTORY          # Share history between all sessions.
unsetopt BEEP                 # Turn off all beeping
unsetopt BG_NICE              # Don't run all background jobs at a lower priority.
unsetopt CHECK_JOBS           # Don't report on jobs when shell exit.
unsetopt CLOBBER              # Do not overwrite existing files with > and >>.
unsetopt FLOW_CONTROL         # Disable start/stop characters in shell editor.
unsetopt HUP                  # Don't kill jobs on shell exit.
unsetopt MAIL_WARNING         # Don't print a warning message if a mail file has been accessed.
unsetopt MENU_COMPLETE        # Do not auto select the first completion entry.
unsetopt TRANSIENT_RPROMPT    # only show the rprompt on the current prompt

zmodload zsh/terminfo

# Load the completion system ignoring insecure directories.
autoload -Uz compinit

# Load the prompt system
autoload -U promptinit

# Allow command line editing in an external editor.
autoload -Uz edit-command-line
zle -N edit-command-line

# }}}
# Styles {{{
if (( $+commands[dircolors] )); then
    # GNU Core Utilities
    alias ls='ls --group-directories-first'
    if [[ -s "$HOME/.dir_colors" ]]; then
        eval "$(dircolors "$HOME/.dir_colors")"
    else
        eval "$(dircolors)"
    fi
    alias ls="$aliases[ls] --color=auto"
else
    # BSD Core Utilities
    # Define colors for BSD ls.
    export LSCOLORS='exfxcxdxbxGxDxabagacad'

    # Define colors for the completion system.
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=36;01:cd=33;01:su=31;40;07:sg=36;40;07:tw=32;40;07:ow=33;40;07:'

    alias ls='ls -G'
fi

# Prompt Styles
zstyle ':prompt:addon:openstack:tenant:default' color 'green'
zstyle ':prompt:addon:openstack:tenant:openstack' color 'yellow'
zstyle ':prompt:addon:openstack:user:default' color 'green'
zstyle ':prompt:addon:openstack:user:pi_admin' color 'yellow'
zstyle ':prompt:addon:openstack:user:admin' color 'red'
zstyle ':prompt:addon:openstack:token:used' color 'magenta'
zstyle ':prompt:addon:openstack:token:none' color 'blue'
zstyle ':prompt:addon:openstack:token:used' symbol '!'
zstyle ':prompt:addon:openstack:token:none' symbol '@'
zstyle ':prompt:addon:openstack:tenant:openstack' color 'yellow'


# Completion {{{
# Use caching to make completion for commands such as dpkg and apt usable.
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"

# Case-insensitive (all), partial-word, and then substring completion.
if zstyle -t ':prezto:module:completion:*' case-sensitive; then
  zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
  setopt CASE_GLOB
else
  zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
  unsetopt CASE_GLOB
fi

# Group matches and describe.
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

# Fuzzy match mistyped completions.
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Increase the number of errors based on the length of the typed word.
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# Don't complete unavailable commands.
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# Array completion element sorting.
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Directories
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true

# History
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# Environmental Variables
# zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

# Populate hostname completion.
zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${=${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
  ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

# Don't complete uninteresting users...
zstyle ':completion:*:*:*:users' ignored-patterns \
  adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
  dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
  hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
  mailman mailnull mldonkey mysql nagios \
  named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
  operator pcap postfix postgres privoxy pulse pvm quagga radvd \
  rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs '_*'

# ... Unless we really want to.
zstyle '*' single-ignored show

# Ignore multiple entries.
zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

# Kill
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

# Media Players
zstyle ':completion:*:*:mpg123:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:mpg321:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:ogg123:*' file-patterns '*.(ogg|OGG|flac):ogg\ files *(-/):directories'
zstyle ':completion:*:*:mocp:*' file-patterns '*.(wav|WAV|mp3|MP3|ogg|OGG|flac):ogg\ files *(-/):directories'

# Mutt
if [[ -s "$HOME/.mutt/mutt.aliases" ]]; then
  zstyle ':completion:*:*:mutt:*' menu yes select
  zstyle ':completion:*:mutt:*' users ${${${(f)"$(<"$HOME/.mutt/mutt.aliases")"}#alias[[:space:]]}%%[[:space:]]*}
fi

# SSH/SCP/RSYNC
zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'
# }}}
# }}}
# Functions {{{
typeset -U myfpath
myfpath=(
    ${zdir}/Functions
    ${HOME}/.local/ZFunctions
)
myfpath=($^myfpath(N-/))
fpath=($myfpath $fpath)

# Autoload any functions that are set executable in added paths
for fp in $myfpath; do
    for func in $fp/*(*N:t:r); do
        autoload $func
    done
done
unset myfpath

# Load OpenStack fuctions, if we have them. {{{
if [[ -d $HOME/.openstack ]]; then
    # For files marked +x in ~/.openstack
    for file in $HOME/.openstack/*(*); do
        source $file
    done
    # Add OpenStack scripts dir to path
    if [[ -d $HOME/.openstack/scripts ]];then
        path+=$HOME/.openstack/scripts
    fi
fi # }}}
# Makes a directory and changes to it. {{{
function mkdcd {
  test -n "$1" && mkdir -p "$1" && builtin cd "$1"
} # }}}
# Changes to a directory and lists its contents. {{{
function cdls {
  builtin cd "$argv[-1]" && ls "${(@)argv[1,-2]}"
} # }}}
# On empty line, run `bg` else hold command {{{
fancy-ctrl-z () {
    if [[ $#BUFFER -eq 0 ]]; then
        bg
        zle redisplay
    else
        zle push-input
    fi
}
zle -N fancy-ctrl-z # }}}
# Auto expand global aliases {{{
globalias() {
   if [[ $LBUFFER =~ ' [A-Z0-9]+$' ]]; then
     zle _expand_alias
     zle expand-word
   fi
   zle self-insert
}
zle -N globalias # }}}
# Clear Line {{{
clear-line() {
    LBUFFER=""
}
zle -N clear-line # }}}
# Recompile Zsh configuration {{{
function zsh_recompile() {
    autoload -U zrecompile

    test -f ~/.zshrc && zrecompile -p ~/.zshrc
    test -f ~/.zshrc.zwc.old && rm -f ~/.zshrc.zwc.old

    for f in ~/.zsh/**/*.zsh; do
        test -f $f && zrecompile -p $f
        test -f $f.zwc.old && rm -f $f.zwc.old
    done

    test -f ~/.zcompdump && zrecompile -p ~/.zcompdump
    test -f ~/.zcompdump.zwc.old && rm -f ~/.zcompdump.zwc.old

    source ~/.zshrc
} # }}}
# Exposes information about the Zsh Line Editor via the $editor_info associative array. {{{
function editor-info {
  # Clean up previous $editor_info.
  unset editor_info
  typeset -gA editor_info

  if [[ "$KEYMAP" == 'vicmd' ]]; then
    zstyle -s ':editor:info:keymap:alternate' format 'REPLY'
    editor_info[keymap]="$REPLY"
  else
    zstyle -s ':editor:info:keymap:primary' format 'REPLY'
    editor_info[keymap]="$REPLY"

    if [[ "$ZLE_STATE" == *overwrite* ]]; then
      zstyle -s ':editor:info:keymap:primary:overwrite' format 'REPLY'
      editor_info[overwrite]="$REPLY"
    else
      zstyle -s ':editor:info:keymap:primary:insert' format 'REPLY'
      editor_info[overwrite]="$REPLY"
    fi
  fi

  unset REPLY

  zle reset-prompt
  zle -R
}
zle -N editor-info # }}}
# Updates editor information when the keymap changes. {{{
function zle-keymap-select {
  zle editor-info
}
zle -N zle-keymap-select # }}}
# Enables terminal application mode and updates editor information. {{{
function zle-line-init {
  # The terminal must be in application mode when ZLE is active for $terminfo
  # values to be valid.
  if (( $+terminfo[smkx] )); then
    # Enable terminal application mode.
    echoti smkx
  fi

  # Update editor information.
  zle editor-info
}
zle -N zle-line-init # }}}
# Disables terminal application mode and updates editor information. {{{
function zle-line-finish {
  # The terminal must be in application mode when ZLE is active for $terminfo
  # values to be valid.
  if (( $+terminfo[rmkx] )); then
    # Disable terminal application mode.
    echoti rmkx
  fi

  # Update editor information.
  zle editor-info
}
zle -N zle-line-finish # }}}
# Toggles emacs overwrite mode and updates editor information. {{{
function overwrite-mode {
  zle .overwrite-mode
  zle editor-info
}
zle -N overwrite-mode

# Enters vi insert mode and updates editor information.
function vi-insert {
  zle .vi-insert
  zle editor-info
}
zle -N vi-insert # }}}
# Moves to the first non-blank character then enters vi insert mode and updates {{{
# editor information.
function vi-insert-bol {
  zle .vi-insert-bol
  zle editor-info
}
zle -N vi-insert-bol # }}}
# Enters vi replace mode and updates editor information. {{{
function vi-replace  {
  zle .vi-replace
  zle editor-info
}
zle -N vi-replace # }}}
# Displays an indicator when completing. {{{
function expand-or-complete-with-indicator {
  local indicator
  zstyle -s ':editor:info:completing' format 'indicator'
  print -Pn "$indicator"
  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-indicator # }}}
# Inserts 'sudo ' at the beginning of the line. {{{
function prepend-sudo {
  if [[ "$BUFFER" != su(do|)\ * ]]; then
    BUFFER="sudo $BUFFER"
    (( CURSOR += 5 ))
  fi
}
zle -N prepend-sudo # }}}
# }}}
# External Modules {{{
# zsh-completions
if [[ -d ${zdir}/zsh-completions ]]; then
    fpath=(${zdir}/zsh-completions $fpath)
fi

# zsh-syntax-highlighting
if [[ -d ${zdir}/zsh-syntax-highlighting ]]; then
    source ${zdir}/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# zsh-history-substring-search
if [[ -d ${zdir}/zsh-history-substring-search ]]; then
    source ${zdir}/zsh-history-substring-search/zsh-history-substring-search.zsh
fi
# }}}
# Key Bindings {{{
bindkey -v

# zsh-history-substring-search
if (( $+widgets[history-substring-search-up] )); then
    bindkey "$key_info[Up]" history-substring-search-up
    bindkey "$key_info[Down]" history-substring-search-down
    bindkey -M vicmd 'k' history-substring-search-up
    bindkey -M vicmd 'j' history-substring-search-down
fi

# Clear Line
bindkey '\el' clear-line

# Edit command in an external editor.
bindkey -M vicmd "v" edit-command-line
bindkey -M viins "^v" edit-command-line

# Undo/Redo
bindkey -M vicmd "u" undo
bindkey -M vicmd "$key_info[Control]R" redo

if (( $+widgets[history-incremental-pattern-search-backward] )); then
  bindkey -M vicmd "?" history-incremental-pattern-search-backward
  bindkey -M vicmd "/" history-incremental-pattern-search-forward
else
  bindkey -M vicmd "?" history-incremental-search-backward
  bindkey -M vicmd "/" history-incremental-search-forward
fi

bindkey -M viins "$key_info[Home]" beginning-of-line
bindkey -M viins "$key_info[End]" end-of-line

bindkey -M viins "$key_info[Insert]" overwrite-mode
bindkey -M viins "$key_info[Delete]" delete-char
bindkey -M viins "$key_info[Backspace]" backward-delete-char

bindkey -M viins "$key_info[Left]" backward-char
bindkey -M viins "$key_info[Right]" forward-char

# Clear screen.
bindkey -M viins "$key_info[Control]L" clear-screen

# Expand command name to full path.
for key in "$key_info[Escape]"{E,e}
    bindkey -M viins "$key" expand-cmd-path

# Duplicate the previous word.
for key in "$key_info[Escape]"{M,m}
    bindkey -M viins "$key" copy-prev-shell-word

# Bind Shift + Tab to go to the previous menu item.
bindkey -M viins "$key_info[BackTab]" reverse-menu-complete

# Complete in the middle of word.
bindkey -M viins "$key_info[Control]I" expand-or-complete

# Display an indicator when completing.
bindkey -M viins "$key_info[Control]I" \
    expand-or-complete-with-indicator

# Insert 'sudo ' at the beginning of the line.
bindkey -M viins "$key_info[Control]X$key_info[Control]S" prepend-sudo

# Auto expand global aliases
bindkey " " globalias
bindkey "^ " magic-space           # control-space to bypass completion
bindkey -M isearch " " magic-space # normal space during searches

# Fancy Ctrl-Z
bindkey '^Z' fancy-ctrl-z
# }}}
# Tmux {{{
if (( $+commands[tmux] )); then
    # Ensure that tmux server is started.
    tmux start-server
    if [[ -z $TMUX && -z $SSH_CONNECTION && $USER != 'root' ]]; then
        # Ensure the Global session is active
        if ! tmux has-session -t "Global" 2> /dev/null; then
            # Create Global session
            tmux new-session -d -s "Global" "weechat"

            # Disable the destruction of the Global session.
            tmux set-option -t "Global" destroy-unattached off &> /dev/null
        fi
        exec tmux
    fi
fi
# }}}
# Prompt {{{
# Prefer Powerline, if available
typeset -U powerlineLocation
powerlineLocation=(
    $(print {$HOME/.local,/usr}/lib{,64}/python*/site-packages/powerline/bindings/zsh/powerline.zsh(N))
    /usr/share/zsh/site-contrib/powerline.zsh
)
# Remove non-existant locations
powerlineLocation=($^powerlineLocation(N-.))
if (( $#powerlineLocation )); then
    source $powerlineLocation[1];
else
    prompt steef
    # Load OpenStack RPrompt, if needed. {{{
    if [[ -d ${HOME}/.openstack ]]; then
        set_rprompt() {
            # Get OpenStack Tennant Name, if set
            if [[ -z $OS_TENANT_NAME ]]; then
                tenant_txt=""
            else
                zstyle -s ":prompt:addon:openstack:tenant:$OS_TENANT_NAME" color color || \
                    zstyle -s ":prompt:addon:openstack:tenant:default" color color
                tenant_txt=" %B%F{green}(%b%F{${color}}$OS_TENANT_NAME%B%F{green})%b%f"
            fi

            # Set marker if using OpenStack token
            if [[ -z $OS_SERVICE_TOKEN ]]; then
                zstyle -s ':prompt:addon:openstack:token:none' color color || \
                    color="blue"
                zstyle -s ':prompt:addon:openstack:token:none' symbol symbol || \
                    symbol="!"
            else
                zstyle -s ':prompt:addon:openstack:token:used' color color || \
                    color="magonta"
                zstyle -s ':prompt:addon:openstack:token:used' symbol symbol || \
                    symbol="@"
            fi
            token="%B%F{${color}}${symbol}%b%f"

            # Set Open Stack Username / Environment, if set
            if [[ -z $OS_USERNAME ]]; then
                tusername=""
            else
                zstyle -s ":prompt:addon:openstack:user:$OS_USERNAME" color color || \
                    zstyle -s ":prompt:addon:openstack:user:default" color color
                tusername="%b%F{${color}}$OS_USERNAME%b%f"
            fi

            if [[ -z $OS_REGION_NAME || -s $tusername ]]; then
                os_prompt=""
            else
                os_prompt="$tusername$token%b%F{green}$OS_REGION_NAME%b%f$tenant_txt"
            fi

            if [[ -z $os_prompt ]]; then
                RPROMPT=""
            else
                RPROMPT="[$os_prompt]"
            fi
        }
        add-zsh-hook precmd set_rprompt
    fi # }}}
fi
# }}}
# Aliases {{{
alias mmv='noglob zmv -W'
alias -g ISODATE='$(date --iso-8601=date)'
alias -g PL='| ${PAGER}'
alias -g PG='| grep -P'
alias -g PE='| egrep'
alias -g PN1='> /dev/null'
alias -g PN2='2> /dev/null'
alias -g PN='&> /dev/null'
alias -s zsh=/bin/zsh
alias -s pl=/bin/perl
alias -s sh=/bin/sh

if (( $+commands[hub] ))        ; then eval $(hub alias -s)                                         ; fi
if (( $+commands[docker] ))     ; then alias docker='sudo docker'                                   ; fi
if (( $+commands[cowsay] ))     ; then alias foxsay='cowsay -nf ~/.local/share/cowsay/cows/fox.cow' ; fi
if (( $+commands[xclip] ))      ; then alias -g CLIP='$(xclip -o -sel clip)'                        ; fi
if (( $+commands[runhaskell] )) ; then alias -s hs=runhaskell                                       ; fi

# Load package manager based Aliases {{{
if (( $+commands[emerge] )); then
    if [[ $EUID != 0 ]]; then
        alias emerge='sudo emerge'
    fi
    if (( $+commands[equery] )); then
            alias equery="noglob equery"
    fi
fi
# }}}
# }}}
# SSH Keys {{{
# The shell globbing pattern below should only return private keys in the
# configuration directory that are marked executable. It should not return any
# other files, even if the are set +x
for key in $(print ${HOME}/.ssh/*~config~authorized_keys~known_hosts~*.pub(*N)); do
    if ! $(ssh-add -l | grep -q $key); then
        ssh-add $key
    fi
done
# }}}
# Read local configuration {{{
if [[ -f ${ZDOTDIR:-$HOME}/.zshrc.local ]]; then
    source ${ZDOTDIR:-$HOME}/.zshrc.local
fi
# }}}

# Everything else is loaded, so it should be safe to initialize the
# completion system. This makes sure any additions where not missed.
compinit -i
promptinit

# Give a Fortune!
if [[ -z $SSH_CONNECTION && $USER != 'root' ]]; then
    fortune
fi

# vim: fdm=marker syntax=zsh
