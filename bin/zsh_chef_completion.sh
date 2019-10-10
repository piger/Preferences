#!/usr/bin/env zsh
# Refresh zsh hostname completion
# NOTE: requires 'moreutils' and 'coreutils' (for gtimeout)

if ~/bin/vpn_status >/dev/null; then
    # gtimeout 2m knife node list 2>/dev/null > /tmp/chef_hosts.new
    gtimeout 2m knife node list > /tmp/chef_hosts.new
    if [[ $(awk 'END { print NR }' /tmp/chef_hosts.new) -lt 5 ]]; then
		exit 1
    fi
    mv /tmp/chef_hosts.new ~/.chef_hosts
fi
