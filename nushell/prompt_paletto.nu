# How to use:
# $ use ./prompt_paletto.nu
# $ $env.PROMPT_COMMAND = { || prompt_paletto }
# $ $env.PROMPT_INDICATOR = { || prompt_paletto indicator }
# $ $env.PROMPT_COMMAND_RIGHT = { || prompt_paletto prompt_right }

const colors = {
    orange: "#C8642A",
    black: "#1B1914",
    mustard: "#CE9C3E",
    grey: "#3A3930",
    green: "#739C70",
    blue: "#548387",
    black2: "#2A2821",
    blue2: "#3D7FA4",
    grey2: "#645D55",
    grey3: "#3A3837",
    cream: "#FAF2CB",
    gold: "#FCBA03",
    red: "#D61C0F",
}

const symbols = {
    separator: (char --unicode "e0b0"),
    git: (char --unicode "f418"),
    half_circle_left: (char --unicode "e0b6"),
    half_circle_right: (char --unicode "e0b4"),
    clock: (char --unicode "f017"),
    prompt: (char --unicode "00bb"),
    computer: (char --unicode "ea7a"),
    dot: (char --unicode "f444"),
    plus: (char --unicode "f067"),
    directory: (char --unicode "f413"),
    error: (char --unicode "2718"),
    dolly: (char --unicode "ed7e"),
    network: (char --unicode "eb01"),
}

export def main [] {
    prompt
}

# indicator returns the prompt displayed right before the input prompt; this is what in bash is tradionally "$" or "#".
export def indicator [] {
    let symbol = match (is-admin) {
        true => "#",
        false => $symbols.prompt,
    }
    (segment "" $colors.gold $"nu ($symbol) ")
}

# prompt_right returns the prompt for the right side of the screen.
export def prompt_right [] {
    match $env.LAST_EXIT_CODE {
        0 => "",
        _ => $"(ansi $colors.red)($symbols.error)(ansi reset) (ansi bo)($env.LAST_EXIT_CODE)(ansi reset)",
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
        (segment $colors.mustard $colors.orange $symbols.separator)
        (current_directory)
        (git_info)
        (segment $colors.grey2 $colors.blue $symbols.separator)
        (segment $colors.grey3 $colors.grey2 $symbols.separator)
        (segment $colors.grey3 $colors.cream $" ($symbols.clock) ($now) ")
        (segment "" $colors.grey3 $"($symbols.half_circle_right) (char newline)")
    ] | str join
}

# segment returns a colored segment for the prompt; imagine the prompt as an array of segments
# that gets joined together before being displayed.
def segment [background: string, foreground: string, text: string] {
    mut attrs = {}
    if not ($background | is-empty) {
        $attrs.bg = $background
     }

    if not ($foreground | is-empty) {
        $attrs.fg = $foreground
    }

    $"(ansi --escape $attrs)($text)(ansi reset)"
}

# computer_name returns the hostname of the machine or, when logged in into a remote system via ssh,
# a user@hostname string.
def computer_name [] {
    let hostname = sys host | get hostname

    match ($env.SSH_CONNECTION? | default "") {
        # not set
        "" => (segment $colors.orange $colors.black $"($symbols.computer) ($hostname) "),

        # is set
        _ => (segment $colors.orange $colors.black $"($symbols.network) ($env.USER)@($hostname)"),
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
    segment $colors.mustard $colors.grey $" ($symbols.directory) ($cwd) "
}

def prompt_start [] {
    segment "" $colors.orange $symbols.half_circle_left
}

def git_info [] {
    let info = gstat
    if ($info | get repo_name) == "no_repository" {
        [
            (segment $colors.green $colors.mustard $symbols.separator)
            (segment $colors.blue $colors.green $symbols.separator)
        ] | str join
    } else {
        [
            (segment $colors.green $colors.mustard $symbols.separator)
            (segment $colors.green $colors.black2 $" ($symbols.git) ($info | get branch)")
            (segment $colors.green $colors.black2 " ")
            (segment $colors.blue $colors.green $symbols.separator)
        ] | str join
    }
}
