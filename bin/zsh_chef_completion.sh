#!/usr/bin/env zsh
# Refresh zsh hostname completion
{ knife node list; jq -r .name ~/src/chef-repo/data_bags/hypervisors/*.json } > ~/.chef_hosts.new && mv ~/.chef_hosts.new ~/.chef_hosts
