# Zsh configuration for the Chef node name completion

zmodload zsh/mapfile
if [[ "$CHEF_NODES_DIR" != "" && -d "$CHEF_NODES_DIR" && -e ~/.chef_hosts ]]; then
    zstyle -s 'completion:*:hosts' hosts _ssh_hosts
    #_ssh_hosts+=($(find "$CHEF_NODES_DIR" -name '*.json' -exec cat {} \; | jq -r ".name"))
    _ssh_hosts=("${(f@)mapfile[~/.chef_hosts]}")
    zstyle 'completion:*:hosts' hosts $_ssh_hosts
fi
