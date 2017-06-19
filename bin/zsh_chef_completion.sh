#!/usr/bin/env zsh
# Refresh zsh hostname completion
# NOTE: requires 'moreutils' and 'coreutils' (for gtimeout)

gtimeout 2m knife node list 2>/dev/null > /tmp/chef_hosts.new
if [[ $(awk 'END { print NR }' /tmp/chef_hosts.new) -lt 5 ]]; then
    exit 1
fi

jq -r .name ~/src/chef-repo/data_bags/hypervisors/*.json >> /tmp/chef_hosts.new
mv /tmp/chef_hosts.new ~/.chef_hosts
