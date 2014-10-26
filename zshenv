#                                   ________,cccccccc.__________
#  cc$$FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF""""""""'''''''`"""""""""?$F
#   ]$[                      ___                              d$'
#   ]$[                    ,dF"?F$c.                          d$
#   ]$[                   ,$[     7$[                        ,$F
#    $h              ,c.  d$      `$'    ,$F           _     J$'
#    $h        _,ccc$$$    ?h.           ]$[          d$     d$
#    d$      c$F"'  dF      `?hc_        ]$[          d$     d$
#    ?$.          ,dF         ``?$c.     ]$[          d$     d$
#    ]$[         d$'              `?$c   ]$[          $h     dF
#    ]$[       ,$F                  `?$  ,$ccccccccccp$[     $h
#    ]$[    _d$F'         d$'       ,$F  ]$[         d$      $F
#     $h  dd$$           ]$[       ,$F   ]$[         d$     ]$[
#     ?h   `"?F$ccc._     ?$ccccccd$F    ]$[         $h     ]$[
#     d$         ``"?F$cc   `'''''        '          $h     d$
#     d$               `'                            $F     d$
#     ?$                                            ]$[     $F
#     ]$[                                           d$'     $h
#     ]$[                                            '     ]$[
#     ]$[                         _______________,cccccccccd$[
#     ]$[           ____,cccccdFFFF"""""""""""""""'''''''''`'
#     `$hccccccdFFFFF""""''''
#  .-------------------------------------------------------.
#  | Questo file verrà' sempre letto da zsh e dovrà' quindi|
#  |contenere tutti le impostazioni che potrebbero influire|
#  |         sui job in crontab, su script eseguiti, etc...|
#  `-------------------------------------------------------'

# ZDOTDIR is HOME by default, by I prefer to keep everything inside a single
# directory.
# ZDOTDIR=${HOME}/.zsh


# PATH handling {{{
# My default PATH *must* contain all 'sbin' directories (cause I use sudo a lot
# and I want completion); then there are some issues:
# - on slackware the default zsh init files in /etc overwrite any PATH value.
# - on OSX if you use MacPorts or Homebrew you should put '/usr/local/{s,}bin
# before /usr/{s,}bin.
#
# PATH and path refers to the same variable (tied), so as MANPATH and manpath.
# The attribute flag -U is to keep unique values inside an array.
typeset -U path manpath fpath cdpath
path=(
    $path /sbin /bin /usr/sbin /usr/bin /usr/X11R6/bin 
    /usr/games /usr/local/sbin /usr/local/bin /opt/local/bin
    /opt/bin
)

# Go, which is tipically installed in /usr/local/go; note: on OSX this can be
# handled with /etc/paths.d and path_helper(8).
if [[ -d /usr/local/go ]]; then
    path+=/usr/local/go/bin
fi

# If you have a ~/bin directory, add it to PATH (here I use the path parameter
# instead of the PATH environment variable).
[[ -d ~/bin ]] && path+=~/bin
# same for ~/local
LOCAL_PKGS=~/local
[[ -d $LOCAL_PKGS/bin ]] && path+=$LOCAL_PKGS/bin
# }}}


# Use ssh for any remote operation involving CVS or rsync
export CVS_RSH="ssh"
export RSYNC_RSH="ssh"

# Format string for 'time' command:
TIMEFMT="Real: %E User: %U System: %S Percent: %P Cmd: %J"

# Se la shell e' interattiva, usa 026.
#if [[ $- == *i* ]]; then
#	# u+rw, g+r, o+NIENTE
#	umask 026
#else
#	umask 022
#fi

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

# vim: ft=zsh
