# nushell config

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

use ([$nu.home-path "Preferences/nushell/prompt_paletto.nu"] | path join)
$env.PROMPT_COMMAND = { || prompt_paletto }
$env.PROMPT_INDICATOR = { || prompt_paletto indicator }
$env.PROMPT_COMMAND_RIGHT = { || prompt_paletto prompt_right }

if not (which vivid | is-empty) {
    $env.LS_COLORS = (vivid generate jellybeans)
}

