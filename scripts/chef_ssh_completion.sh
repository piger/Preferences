#!/bin/bash
# This script will populate $HOME/.chef_hosts with a list of all servers known to chef, for zsh completion.

find $HOME/src/chef-repo-nodes -name '*.json' | xargs cat | jq -r '.name' > $HOME/.chef_hosts.new && mv $HOME/.chef_hosts.new $HOME/.chef_hosts
