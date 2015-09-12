#
# Contains functions for identifying the status of a Darcs VCS Repo
#
# Authors:
#   Tim Millican <tim.millican@linux.com>
#

# Get the name of the branch we are on
function darcs_prompt() {
    zstyle -s ':prompt:addon:vcs:darcs' enabled show_darcs || show_darcs='true'
    if [[ ${show_darcs} == 'true' && -n $(darcs log --count 2> /dev/null) ]]; then
        zstyle -s ':prompt:addon:vcs:darcs' prefix prefix || prefix=''
        zstyle -s ':prompt:addon:vcs:darcs' suffix suffix || suffix=''
        zstyle -s ':prompt:addon:vcs:darcs:status:dirty' prefix dprefix || dprefix=''
        zstyle -s ':prompt:addon:vcs:darcs:status:dirty' suffix dsuffix || dsuffix='!'
        zstyle -s ':prompt:addon:vcs:darcs:status:clean' prefix cprefix || cprefix=''
        zstyle -s ':prompt:addon:vcs:darcs:status:clean' suffix csuffix || csuffix=''

        # Print branch (or SHA) with indications of clean/dirty/untracked
        if [[ -n "$(command darcs whatsnew --summary|grep -P '^A|^R|^M')" ]]; then
            print -n " ${prefix}${dprefix}*${dsuffix}${suffix}"
        else
            print -n " ${prefix}${cprefix}*${csuffix}${suffix}"
        fi
    fi
}
