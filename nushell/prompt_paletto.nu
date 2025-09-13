# How to use:
# $ use ./prompt_paletto.nu
# $ $env.PROMPT_COMMAND = { || prompt_paletto }
# $ $env.PROMPT_INDICATOR = { || prompt_paletto indicator }
# $ $env.PROMPT_COMMAND_RIGHT = { || prompt_paletto prompt_right }

const colors = {
    orange: "#C8642A",
    lilac: "#a76cba",
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
    light_blue: "#5ed7ff",
}

# Nerd Fonts: https://www.nerdfonts.com/cheat-sheet
const symbols = {
    # nf-pl-left_hard_divider 
    separator: (char --unicode "e0b0"),

    # nf-oct-git_branch 
    git: (char --unicode "f418"),

    # nf-ple-left_half_circle_thick 
    half_circle_left: (char --unicode "e0b6"),

    # nf-ple-right_half_circle_thick 
    half_circle_right: (char --unicode "e0b4"),

    # nf-fa-clock 
    clock: (char --unicode "f017"),

    # https://en.wikipedia.org/wiki/Guillemet "»"
    prompt: (char --unicode "00bb"),

    # nf-cod-vm 
    computer: (char --unicode "ea7a"),

    # nf-oct-dot_fill 
    dot: (char --unicode "f444"),

    # nf-md-dots_vertical
    dots_vertical: (char --unicode "f01d9"),

    # nf-fa-plus 
    plus: (char --unicode "f067"),

    # nf-oct-file_directory 
    directory: (char --unicode "f413"),

    # U+2718 : HEAVY BALLOT ✘
    error: (char --unicode "2718"),

    # nf-fa-dolly 
    dolly: (char --unicode "ed7e"),

    # nf-cod-globe 
    network: (char --unicode "eb01"),
}

export def main [] {
    prompt
}

# indicator returns the prompt displayed right before the input prompt; this is what in bash is tradionally "$" or "#".
export def indicator [] {
    let prompt_char = match (is-admin) {
        true => "#",
        false => $symbols.prompt,
    }

    let modified = if (gstat | get wt_modified) > 0 {
        [$symbols.dot " "] | str join
    } else {
        ""
    }

    let added = if (gstat | get idx_modified_staged) > 0 {
        [$symbols.plus " "] | str join
    } else {
        ""
    }

    [
        (ansi $colors.gold)
        "nu "
        (ansi $colors.blue2)
        $added
        $modified
        (ansi $colors.gold)
        $prompt_char
        " "
    ] | str join
}

export def simple_indicator [] {
    let prompt_char = match (is-admin) {
        true => "#",
        false => $symbols.prompt,
    }

    let now = date now | format date "%H:%M"

    [
        (ansi $colors.light_blue)
        $symbols.clock
        (ansi reset)
        " "
        $now
        $" ($symbols.dots_vertical) "
        (ansi $colors.light_blue)
        $symbols.directory
        " "
        (ansi $colors.cream)
        (current_directory)
        " "
        (ansi $colors.gold)
        $prompt_char
        " "
        (ansi reset)
    ] | str join
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
        (segment --bg $colors.mustard --fg $colors.orange $symbols.separator)
        (segment_directory)
        (git_info)
        # end of the prompts' first line: colored arrows and clock
        (segment --bg $colors.grey2 --fg $colors.blue $symbols.separator)
        (segment --bg $colors.grey3 --fg $colors.grey2 $symbols.separator)
        (segment --bg $colors.grey3 --fg $colors.cream $" ($symbols.clock) ($now) ")
        (segment --fg $colors.grey3 $"($symbols.half_circle_right) (char newline)")
    ] | str join
}

# segment returns a colored segment for the prompt; imagine the prompt as an array of segments
# that gets joined together before being displayed.
def segment [
    text: string
    --bg: string
    --fg: string
] {
    mut attrs = {}
    if not ($bg | is-empty) {
        $attrs.bg = $bg
     }

    if not ($fg | is-empty) {
        $attrs.fg = $fg
    }

    $"(ansi --escape $attrs)($text)(ansi reset)"
}

# computer_name returns the hostname of the machine or, when logged in into a remote system via ssh,
# a user@hostname string.
def computer_name [] {
    let hostname = sys host | get hostname

    match ($env.SSH_CONNECTION? | default "") {
        # not set
        "" => (segment --bg $colors.orange --fg $colors.black $"($symbols.computer) ($hostname) "),

        # is set
        _ => (segment --bg $colors.orange --fg $colors.black $"($symbols.network) ($env.USER)@($hostname)"),
    }
}

# current_directory returns the current directory, abbreviated if it's a descendant of the home directory of the
# current user.
def current_directory [] {
    # like it's done here: https://github.com/nushell/nu_scripts/blob/main/modules/prompt/panache-git.nu#L28
    let cwd_relative = (do --ignore-errors { pwd | path relative-to $nu.home-path })

    if (pwd) == $nu.home-path {
        "~"
    } else if ($cwd_relative | is-empty) {
        pwd
    } else {
        $"~/($cwd_relative)"
    }
}

def segment_directory [] {
    segment --bg $colors.mustard --fg $colors.grey $" ($symbols.directory) (current_directory) "
}

def prompt_start [] {
    segment --fg $colors.orange $symbols.half_circle_left
}

def git_info [] {
    let info = gstat
    if ($info | get repo_name) == "no_repository" {
        [
            (segment --bg $colors.green --fg $colors.mustard $symbols.separator)
            (segment --bg $colors.blue --fg $colors.green $symbols.separator)
        ] | str join
    } else {
        [
            (segment --bg $colors.green --fg $colors.mustard $symbols.separator)
            (segment --bg $colors.green --fg $colors.black2 $" ($symbols.git) ($info | get branch)")
            (segment --bg $colors.green --fg $colors.black2 " ")
            (segment --bg $colors.blue --fg $colors.green $symbols.separator)
        ] | str join
    }
}
