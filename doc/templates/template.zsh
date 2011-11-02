#!/usr/bin/env zsh

setopt NO_UNSET		# unset parameters are treated as errors (set -u) -> "parameter not set"
setopt ERR_EXIT		# If  a  command  has  a non-zero exit status, execute the ZERR trap, if set, and exit.

# Example inspired by http://www.chodorowski.com/projects/zws/
parse_options() {
	o_dir=(-d /path/to)
	o_help=

	zparseopts -K -- d:=o_dir h=o_help
	if [[ $? != 0 || "$o_help" != "" ]]; then
		echo Usage: $(basename "$0") "[-d DIR]"
		exit 1
	fi

	dir=$o_dir[2]
}
parse_options $*
