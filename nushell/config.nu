# nushell config
#
# Run "config nu" and add "source <path to this file>".

# having a const for the nushell directory in Preferences allows to use "source" and "use".
# See also:
# - https://www.nushell.sh/book/configuration.html#using-constants
# - https://www.nushell.sh/book/how_nushell_code_gets_run.html#multiline-repl-commandlines
const nu_dotfiles_dir = path self | path dirname

# git plugin
plugin add nu_plugin_gstat
plugin use gstat

# configure the plugin garbage collector to not unload gstat
# https://www.nushell.sh/book/plugins.html#plugin-garbage-collector
$env.config.plugin_gc = {
    plugins: {
        gstat: {
            enabled: false
        }
    }
}

use ([$nu_dotfiles_dir prompt_paletto.nu] | path join)
$env.PROMPT_COMMAND = { || prompt_paletto }
$env.PROMPT_INDICATOR = { || prompt_paletto indicator }
$env.PROMPT_COMMAND_RIGHT = { || prompt_paletto prompt_right }
$env.TRANSIENT_PROMPT_COMMAND = { || }
$env.TRANSIENT_PROMPT_INDICATOR = { || prompt_paletto simple_indicator }

if not (which vivid | is-empty) {
    $env.LS_COLORS = (vivid generate jellybeans)
}

# direnv support
# https://github.com/nushell/nu_scripts/blob/main/nu-hooks/nu-hooks/direnv/config.nu
def --env direnv_hook [] {
    if (which direnv | is-empty) {
        return
    }

    direnv export json | from json | default {} | load-env
    # Direnv outputs $PATH as a string, but nushell silently breaks if isn't a list-like table.
    # The following behemoth of Nu code turns this into nu's format while following the standards of how to handle quotes, use it if you need quote handling instead of the line below it:
    # $env.PATH = $env.PATH | parse --regex ('' + `((?:(?:"(?:(?:\\[\\"])|.)*?")|(?:'.*?')|[^` + (char env_sep) + `]*)*)`) | each {|x| $x.capture0 | parse --regex `(?:"((?:(?:\\"|.))*?)")|(?:'(.*?)')|([^'"]*)` | each {|y| if ($y.capture0 != "") { $y.capture0 | str replace -ar `\\([\\"])` `$1` } else if ($y.capture1 != "") { $y.capture1 } else $y.capture2 } | str join }
    $env.PATH = $env.PATH | split row (char env_sep)
}

$env.config.hooks = {
    env_change: {
        PWD: [
            { || direnv_hook },
        ]
    }
}

# Disable "linking" files in ls
$env.config.shell_integration.osc8 = false

# I'll stick with eza, for now.
alias nu-ls = ls
alias ls = eza --icons --color-scale --mounts --group-directories-first

$env.config.buffer_editor = "vim"
$env.config.show_banner = false

# zoxidue support
# zoxide init nushell out> zoxide.nu
source ([$nu_dotfiles_dir zoxide.nu] | path join)
