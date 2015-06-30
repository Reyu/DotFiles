#
# Contains functions for identifying the status of a Git VCS Repo
#
# Authors:
#   Tim Millican <tim.millican@linux.com>
#

# Get the name of the branch we are on
function git_prompt() {
    zstyle -s ':prompt:addon:vcs:git' enabled show_git || show_git='true'
    if [[ $show_git == 'true' && -n $(git_branch) ]]; then
        zstyle -s ':prompt:addon:vcs:git' prefix prefix || prefix='on '
        zstyle -s ':prompt:addon:vcs:git' suffix suffix || suffix=''
        zstyle -s ':prompt:addon:vcs:git:status:untracked' prefix uprefix || uprefix=''
        zstyle -s ':prompt:addon:vcs:git:status:untracked' suffix usuffix || usuffix='?'
        zstyle -s ':prompt:addon:vcs:git:status:untracked' enabled uenabled || uenabled='true'
        zstyle -s ':prompt:addon:vcs:git:status:dirty' prefix dprefix || dprefix=''
        zstyle -s ':prompt:addon:vcs:git:status:dirty' suffix dsuffix || dsuffix='!'
        zstyle -s ':prompt:addon:vcs:git:status:clean' prefix cprefix || cprefix=''
        zstyle -s ':prompt:addon:vcs:git:status:clean' suffix csuffix || csuffix=''
        zstyle -s ":prompt:addon:vcs:git:options${PWD:gs/\//:/}" submodules submodules || submodules='none'

        # Print branch (or SHA) with indications of clean/dirty/untracked
        FLAGS=('--porcelain', "--ignore-submodules=$submodules")
        if [[ $(command git status --porcelain --ignore-submodules=$submodules --untracked-files=no|grep -qP '^..') ]]; then
            print " $prefix$dprefix$(git_branch)$dsuffix$suffix"
        elif [[ $(command git status --porcelain --ignore-submodules=$submodules --untracked-files=normal|grep -qP '^\?\?') && $uenabled == 'true' ]]; then
            print " $prefix$uprefix$(git_branch)$usuffix$suffix"
        else
            print " $prefix$cprefix$(git_branch)$csuffix$suffix"
        fi

        # Print indications of remote status, if availible
        git_remote_status
    fi
}

# Get the name of the current branch, or the SHA sum of the current commit
function git_branch() {
        ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
            ref=$(command git rev-parse --short HEAD 2> /dev/null) || return 0
        print ${ref#refs/heads/}
}

# get the difference between the local and remote branches
git_remote_status() {
    remote=${$(command git rev-parse --verify $(git_branch)@{upstream} --symbolic-full-name 2>/dev/null)/refs\/remotes\/}
    if [[ -n ${remote} ]] ; then
        zstyle -s ':prompt:addon:vcs:git:remote:ahead' prefix aprefix || aprefix="+"
        zstyle -s ':prompt:addon:vcs:git:remote:ahead' suffix asuffix || asuffix=""
        zstyle -s ':prompt:addon:vcs:git:remote:behind' prefix bprefix || bprefix="-"
        zstyle -s ':prompt:addon:vcs:git:remote:behind' suffix bsuffix || bsuffix=""

        ahead=$(command git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
        behind=$(command git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)

        if [ $ahead -gt 0 ]; then
            print " $aprefix$ahead$asuffix"
        fi
        if [ $behind -gt 0 ]; then
            print " $bprefix$behind$bsuffix"
        fi
    fi
}
