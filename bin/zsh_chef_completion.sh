#!/usr/bin/env zsh
# Refresh zsh hostname completion
# NOTE: requires 'moreutils' and 'coreutils' (for gtimeout)

{ gtimeout 2m knife node list 2>/dev/null && jq -r .name ~/src/chef-repo/data_bags/hypervisors/*.json } | ifne tee ~/.chef_hosts.new > /dev/null
# replace if size greater than 0
[[ -s ~/.chef_hosts.new ]] && mv ~/.chef_hosts.new ~/.chef_hosts
# cleanup
[[ -e ~/.chef_hosts.new ]] && rm ~/.chef_hosts.new
