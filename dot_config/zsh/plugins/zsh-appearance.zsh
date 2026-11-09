# zsh-appearance
# A zsh thing to switch the theme of command-line utilities to dark or light mode
# depending on the state of the desktop environment.

# _zsh_appearance_kde_is_dark_mode detects the current appearance on KDE
_zsh_appearance_kde_is_dark_mode() {
    local current_theme
    current_theme=$(plasma-apply-colorscheme -l | grep "(current color scheme)" | awk '{ print $2 }')
    if [[ ${(L)current_theme} = *dark ]]; then
        return 0
    fi
    return 1
}

# _zsh_appearance_macos_is_dark_mode detects the current appearance on macOS
# TODO: untested
_zsh_appearance_macos_is_dark_mode() {
    # https://stackoverflow.com/a/79542955
    local result
    result=$(osascript -e 'tell application "System Events" to tell appearance preferences to get dark mode')
    if [[ $result == true ]]; then
        return 0
    fi
    return 1
}

# _zsh_appearance_detect_command is the shell function executed to detect the appearance
# in the current desktop environment session
_zsh_appearance_detect_command=""
if [[ $OSTYPE == linux* ]]; then
    _zsh_appearance_detect_command="_zsh_appearance_kde_is_dark_mode"
elif [[ $OSTYPE == darwin* ]]; then
    _zsh_appearance_detect_command="_zsh_appearance_macos_is_dark_mode"
fi

# _zsh_appearance_current_mode is an internal variable
_zsh_appearance_current_mode=""

# _zsh_appearance_check_every tells this script how often to check for a change in appearance (seconds).
_zsh_appearance_check_every=$(( 60 * 5 ))

# _zsh_appearance_dark_mode_fn must be set to a shell function that sets the dark mode themes
_zsh_appearance_dark_mode_fn="dark_mode"

# _zsh_appearance_light_mode_fn must be set to a shell function that sets the light mode themes
_zsh_appearance_light_mode_fn="light_mode"

# _zsh_appearance_periodic_check is called periodically to check if the appearance changed
# in the current shell session
_zsh_appearance_periodic_check() {
    local current_mode

    # don't do anything if this command is not configured
    if [[ -z $_zsh_appearance_detect_command ]]; then
        return
    fi

    if ${_zsh_appearance_detect_command}; then
        current_mode="dark"
    else
        current_mode="light"
    fi

    if [[ $current_mode != $_zsh_appearance_current_mode ]]; then
        _zsh_appearance_current_mode=$current_mode
        if [[ $_zsh_appearance_current_mode == "dark" ]]; then
            $_zsh_appearance_dark_mode_fn
        else
            $_zsh_appearance_light_mode_fn
        fi
    fi
}

# NOTE: this variable is global in zsh, so every hook in the periodic variable and
# periodic_functions array will be executed periodically according to *THIS* value.
export PERIOD=$_zsh_appearance_check_every

autoload -Uz add-zsh-hook
add-zsh-hook periodic _zsh_appearance_periodic_check
_zsh_appearance_periodic_check
