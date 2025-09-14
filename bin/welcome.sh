#!/usr/bin/env zsh

os_logo() {
    if which fastfetch >/dev/null; then
        fastfetch
    elif [[ $OSTYPE = darwin* ]]; then
	zsh $HOME/Preferences/zsh/functions/apple-logo
    fi
}

fortune-classic() {
    if (( $+commands[fortune] )); then
        printf "\e[3m\e[1mQuote of the day\e[0m\n"

        if [[ -d "$FORTUNES_DIRECTORY" ]]; then
            fortune -s -e "$FORTUNES_DIRECTORY"
        else
            fortune -s -e
        fi
        echo ""
    fi
}

fortune-tips() {
    if [[ -d ${PERSONAL_CODE_DIR}/cli-tips-fortune/ ]]; then
	printf "\e[3m\e[1mTip of the day\e[0m\n"
        fortune ${PERSONAL_CODE_DIR}/cli-tips-fortune/
        echo ""
    fi
}

if which figurine >/dev/null && which boxes >/dev/null; then
    echo
    figurine -f 3d.flf $(hostname)
    echo
    boxes -d parchment <(fortune -s -e "$FORTUNES_DIRECTORY")
    echo
    fortune-tips
else
    os_logo
    fortune-classic
    fortune-tips
fi
