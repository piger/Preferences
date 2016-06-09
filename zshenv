# -*- mode: shell-script -*-
# zshenv
# This file is always executed by zsh so it's mostly useful to set environment
# variables used by non-interactive jobs.

# PATH handling {{{
# PATH and path refers to the same variable (tied), so as MANPATH and manpath.
# The attribute flag -U is to keep unique values inside an array.
typeset -U path manpath fpath cdpath

# set default PATH, keeping any existing system-wide PATH value.
path=(
    $path /sbin /bin /usr/sbin /usr/bin /usr/X11R6/bin 
    /usr/local/sbin /usr/local/bin
)

# Other miscellaneous bin directories.
# If you have a ~/bin directory, add it to PATH (here I use the path parameter
# instead of the PATH environment variable).
[[ -d $HOME/bin ]] && path+=$HOME/bin
# same for ~/local
[[ -d $HOME/local/bin ]] && path+=$HOME/local/bin

[[ -d /opt/bin ]] && path+=/opt/bin
[[ -d /opt/local/bin ]] && path+=/opt/local/bin
[[ -d /usr/games ]] && path+=/usr/games
# }}}


# Use ssh for any remote operation involving CVS or rsync
export CVS_RSH="ssh"
export RSYNC_RSH="ssh"

# Format string for 'time' command:
TIMEFMT="Real: %E User: %U System: %S Percent: %P Cmd: %J"

# Set the umask
if [[ $UID == 0 ]]; then
    umask 077
else
    umask u=rwx,g=rx,o=
fi

# Alias vim and gvim to have it working inside shell functions and such.
# See also: zsh/os/darwin
foreach macvim (/Applications/MacVim.app ~/Applications/MacVim.app); do
  if [[ -e $macvim ]]; then
      alias vim="$macvim/Contents/MacOS/Vim"
      alias gvim="$macvim/Contents/MacOS/MacVim"
      break
  fi
done

# Ruby RVM path (for scripting)
[[ -d $HOME/.rvm/bin ]] && path+=$HOME/.rvm/bin

# Load local zshenv
[[ -f $HOME/.zshenv.local ]] && source $HOME/.zshenv.local

# vim: ft=zsh
