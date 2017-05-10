#!/usr/bin/env zsh
# Refresh zsh hostname completion
#
# NOTE: requires 'moreutils'
{ knife node list && jq -r .name ~/src/chef-repo/data_bags/hypervisors/*.json } | ifne tee ~/.chef_hosts.new > /dev/null
[[ -s ~/.chef_hosts.new ]] && mv ~/.chef_hosts.new ~/.chef_hosts
