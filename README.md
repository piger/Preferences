# Preferences - my dotfiles

A collection of personal configuration files.

## Bootstrap

### Install chezmoi

#### macOS

1. Install [Homebrew](https://brew.sh/)
2. Install chezmoi: `brew install chezmoi`

#### Linux

1. Install chezmoi
    * Check the prebuilt packages [here](https://www.chezmoi.io/install/#download-a-pre-built-linux-package)

### Sync all the dotfiles

1. `chezmoi apply`

### Install packages with Homebrew

1. `brew bundle`

## Updating the dotfiles

1. `git pull` this repository
2. (_optional_) `chezmoi status -v`
3. `chezmoi apply`

## License

See the [LICENSE](LICENSE) file.
