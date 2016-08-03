# -*- shell-script -*-
# zshrc server-specific (GNU/Linux)

### Configuration
setopt auto_pushd
setopt check_jobs
setopt extended_glob
setopt long_list_jobs
setopt numeric_glob_sort
setopt print_exit_value
setopt prompt_subst
setopt pushd_ignore_dups
setopt short_loops
setopt interactive_comments
setopt rematch_pcre
[[ $ZSH_VERSION == <5->.* ]] && setopt combining_chars
setopt NO_beep
setopt NO_flow_control
setopt NO_hup
setopt NO_rm_star_silent

setopt append_history
setopt extended_history
setopt hist_ignore_dups
setopt hist_no_store
setopt hist_verify
setopt hist_ignore_space
setopt hist_expire_dups_first
SAVEHIST=10000
HISTSIZE=12000
HISTFILE=~/.history

PROMPT='[%T] %n@%2m:%3~%(!.#.$) '

### Commands configuration
(( $+commands[dircolors] )) && test -e ~/.dircolors && eval $(dircolors -b ~/.dircolors)

TIMEFMT="%*E real time :: CPU: %P (%U user, %S kernel) :: %J"

LESS="-ciMRx4wJ -z-5"
PAGER=less
export LESS PAGER

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

if (( $+commands[vim] )); then
    export EDITOR=vim
    export VISUAL=vim
else
    export EDITOR=vi
    export VISUAL=vi
fi

export GREP_OPTIONS='--color=auto -D skip'
export GREP_COLOR='1;32'

### Functions
ssl_hashes=( sha512 sha256 sha1 md5 )

for sh in ${ssl_hashes}; do
    eval 'ssl-cert-'${sh}'() {
        emulate -L zsh
        if [[ -z $1 ]] ; then
            printf '\''usage: %s <file>\n'\'' "ssh-cert-'${sh}'"
            return 1
        fi
        openssl x509 -noout -fingerprint -'${sh}' -in $1
    }'
done; unset sh

ssl-cert-fingerprints() {
    emulate -L zsh
    local i
    if [[ -z $1 ]] ; then
        printf 'usage: ssl-cert-fingerprints <file>\n'
        return 1
    fi
    for i in ${ssl_hashes}
        do ssl-cert-$i $1;
    done
}

ssl-cert-info() {
    if [[ -z $1 ]]; then
        echo "Usage: ssl-cert-info <file>"
        exit 1
    fi
    openssl x509 -noout -text -in $1
}

teprego() {
    echo -e "$(tput setaf 1)sudo$(tput sgr0) → $(fc -ln -1)"
    eval "sudo $(fc -ln -1)"
}

### Key bindings
_add-sudo() { [[ $BUFFER != sudo\ * ]] && LBUFFER="sudo $LBUFFER" }
zle -N add-sudo _add-sudo

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '\ee' edit-command-line         # ALT+e per editare la cmdline

# C-e d -- insert current date dd-mm-YYYY
insert-datestamp() { LBUFFER+=${(%):-'%D{%d-%m-%Y}'}; }
zle -N insert-datestamp
bindkey '^Ed' insert-datestamp

### Autoloads
autoload -Uz zmv

### Aliases
alias ls='ls --color=auto -FA'
alias grep='grep --color=auto'
alias lsd="ls -ld *(-/)"                # ls delle sole directory
alias lsa="ls -ld .*"                   # ls dei soli file nascosti
alias lsgrandi="ls -fl *(.oL)"  # ls -lrh --sort=size
alias lsuid="ls -l *(s,S,t)"    # ls solo di file suid/setgid/sticky
alias dirvuote="ls -ld *(/^F)"  # ls solo delle directory vuote
alias dirdu="du -Hhs *(/,@)"    # du -h di ogni dir presente
alias ..='cd ..'
alias ...='cd ./..'
alias -g L='|less'
alias json.tool='python -m json.tool'
alias cheflog='view /var/log/chef/client.log'

### Completion
zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit && compinit

zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:::::' completer _force_rehash _complete _prefix _approximate _ignored
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 ))numeric)'

zstyle ':completion:*:(^approximate):*' matcher-list \
       'r:|[/]=* r:|=* m:{a-z}={A-Z}'

zstyle ':completion:*:*:cd:*' ignored-patterns '(*/|)(CVS)'
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion::*:kill:*:*' command 'ps xf -U $USER -o pid,%cpu,%mem,cmd'
zstyle ':completion::*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

### Bracketed paste mode
bindkey -N paste
bindkey -R -M paste "^@"-"\M-^?" paste-insert
bindkey '^[[200~' _start_paste
bindkey -M paste '^[[201~' _end_paste
bindkey -M paste -s '^M' '^J'

zle -N _start_paste
zle -N _end_paste
zle -N zle-line-init _zle_line_init
zle -N zle-line-finish _zle_line_finish
zle -N paste-insert _paste_insert

function _start_paste() {
  bindkey -A paste main
}

function _end_paste() {
  bindkey -e
  LBUFFER+=$_paste_content
  unset _paste_content
}

function _paste_insert() {
  _paste_content+=$KEYS
}

function _zle_line_init() {
  [[ $TERM == rxvt-unicode || $TERM == xterm || $TERM = xterm-256color || $TERM = screen || $TERM = screen-256color ]] && printf '\e[?2004h'
}

function _zle_line_finish() {
  [[ $TERM == rxvt-unicode || $TERM == xterm || $TERM = xterm-256color || $TERM = screen || $TERM = screen-256color ]] && printf '\e[?2004l'
}

### Local settings
[[ -e ~/.zshrc.local ]] && source ~/.zshrc.local
