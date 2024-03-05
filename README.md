# Preferences - my dotfiles

A collection of personal configuration files.

## Installation

To fetch the external dependencies (a couple of plugins) execute:

``` shell
git submodule update --init
```

Then take a look at `dotfiles.cfg`; it contains a list of files that will be symlinked by the
`colonizza.py` script when executed. The syntax of `dotfiles.cfg` is:

```
<source filename, relative to this repo> [destination file, relative to $HOME]
```

Run `colonizza.py` to set up all the symlinks.

All your configuration files must live in this repository so that you can track them with git.

## Configuration

For *local* configuration sometimes you can use a `.local` file:

- `~/.gitconfig.local`
- `~/.zshenv.local` and `~/.zshrc.local`
- `~/.tmux.conf.local`
- `~/.vimrc.local`
- `~/Preferences/elisp-init/init-local.el`

## Updating

To update all the submodules run:

    git submodule foreach git pull origin master

Then `git add` and commit all the modified submodules directories.

## Notes

Some stuff is ancient, some is outdated, some I don't use anymore. Emacs, zsh and tmux are
used everyday for sure.

Some stuff has been blatantly stolen from other people's dotfiles but I should have included
credits where it made sense.
