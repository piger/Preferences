# How to use:
# $ use ./prompt_paletto.nu
# $ $env.PROMPT_COMMAND = { || prompt_paletto }
# $ $env.PROMPT_INDICATOR = { || prompt_paletto indicator }
# $ $env.PROMPT_COMMAND_RIGHT = { || prompt_paletto prompt_right }

const orange = "#C8642A"
const black = "#1B1914"
const mustard = "#CE9C3E"
const grey = "#3A3930"
const green = "#739C70"
const blue = "#548387"
const black2 = "#2A2821"
const blue2 = "#3D7FA4"
const grey2 = "#645D55"
const grey3 = "#3A3837"
const cream = "#FAF2CB"
const gold = "#FCBA03"
const red = "#D61C0F"

const segment_separator = char --unicode e0b0
const git_symbol = char --unicode f418
const half_circle_left = char --unicode e0b6
const half_circle_right = char --unicode e0b4
const clock = char --unicode f017
const prompt_symbol = char --unicode "00bb"
const computer = char --unicode "ea7a"
const dot = char --unicode f444
const plus = char --unicode f067
const dir_icon = char --unicode f413
const error_symbol = char --unicode "2718"
const dolly_symbol = char --unicode ed7e
const network_symbol = char --unicode eb01

export def main [] {
    prompt
}

# indicator returns the prompt displayed right before the input prompt; this is what in bash is tradionally "$" or "#".
export def indicator [] {
    let symbol = match (is-admin) {
        true => "#",
        false => $prompt_symbol,
    }
    (segment "" $gold $"nu ($symbol) ")
}

# prompt_right returns the prompt for the right side of the screen.
export def prompt_right [] {
    match $env.LAST_EXIT_CODE {
        0 => "",
        _ => $"(ansi $red)($error_symbol)(ansi reset) (ansi bo)($env.LAST_EXIT_CODE)(ansi reset)",
    }
}

# prompt returns the main part of the promt.
def prompt [] {
    # plugin add nu_plugin_gstat
    # plugin use gstat

    let now = date now | format date "%H:%M"

    [
        (prompt_start)
        (computer_name)
        (segment $mustard $orange $segment_separator)
        (current_directory)
        (git_info)
        (segment $grey2 $blue $segment_separator)
        (segment $grey3 $grey2 $segment_separator)
        (segment $grey3 $cream $" ($clock) ($now) ")
        (segment "" $grey3 $"($half_circle_right) (char newline)")
    ] | str join
}

# segment returns a colored segment for the prompt; imagine the prompt as an array of segments
# that gets joined together before being displayed.
def segment [background: string, foreground: string, text: string] {
    mut colors = {}
    if not ($background | is-empty) {
        $colors.bg = $background
     }

    if not ($foreground | is-empty) {
        $colors.fg = $foreground
    }

    $"(ansi --escape $colors)($text)(ansi reset)"
}

# computer_name returns the hostname of the machine or, when logged in into a remote system via ssh,
# a user@hostname string.
def computer_name [] {
    let hostname = sys host | get hostname

    match ($env.SSH_CONNECTION? | default "") {
        # not set
        "" => (segment $orange $black $"($computer) ($hostname) "),

        # is set
        _ => (segment $orange $black $"($network_symbol) ($env.USER)@($hostname)"),
    }
}

# current_directory returns the current directory, abbreviated if it's a descendant of the home directory of the
# current user.
def current_directory [] {
    # like it's done here: https://github.com/nushell/nu_scripts/blob/main/modules/prompt/panache-git.nu#L28
    let cwd_relative = (do --ignore-errors { pwd | path relative-to $nu.home-path })
    let cwd = if (pwd) == $nu.home-path {
        "~"
    } else if ($cwd_relative | is-empty) {
        pwd
    } else {
        $"~/($cwd_relative)"
    }
    segment $mustard $grey $" ($dir_icon) ($cwd) "
}

def prompt_start [] {
    segment "" $orange $half_circle_left
}

def git_info [] {
    let info = gstat
    if ($info | get repo_name) == "no_repository" {
        [
            (segment $green $mustard $segment_separator)
            (segment $blue $green $segment_separator)
        ] | str join
    } else {
        [
            (segment $green $mustard $segment_separator)
            (segment $green $black2 $" ($git_symbol) ($info | get branch)")
            (segment $green $black2 " ")
            (segment $blue $green $segment_separator)
        ] | str join
    }
}
