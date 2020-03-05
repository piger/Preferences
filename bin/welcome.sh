#!/bin/zsh -f

myself=$0:A

logo() {
    if [[ ! -x $HOME/Preferences/bin/imgcat ]]; then
        echo "Missing imgcat"
        apple-logo
        return
    fi

    if [[ ! -d $HOME/Pictures/pixel-art/ ]]; then
        echo "No pixel-art directory"
        apple-logo
        return
    fi

    if [[ $(( RANDOM % 2 )) = 0 ]]; then
        apple-logo
    else
        # devilish
        doodle=$(perl -x $myself)
        $HOME/Preferences/bin/imgcat "$HOME/Pictures/pixel-art/$doodle"
    fi
}

# THIS IS DEVILISH
echo <<'__END__' > /dev/null
#!/usr/bin/perl -w
use List::Util qw/shuffle/;

opendir($dh, "$ENV{\"HOME\"}/Pictures/pixel-art") || die "Cannot open the pixel-art directory";
@pics = grep { /\.(?:gif|png|jpe?g)$/ } readdir($dh);
@pics = shuffle @pics;
print $pics[0];
__END__

fortune-classic() {
    printf "\e[3m\e[1mQuote of the day\e[0m\n"
    # fortune -e serve per scegliere equamente tra tutti i fortune file
    # presenti, a prescindere dalla dimensione.
    if (( $+commands[fortune] )); then
        if [[ -d "$FORTUNES_DIRECTORY" ]]; then
            fortune -s -e "$FORTUNES_DIRECTORY"
        else
            fortune -s -e -o
        fi
    else
        echo "You need to install fortune!"
    fi
}

fortune-tips() {
    printf "\e[3m\e[1mTip of the day\e[0m\n"
    if [[ -d ${PERSONAL_CODE_DIR}/cli-tips-fortune/ ]]; then
        fortune ${PERSONAL_CODE_DIR}/cli-tips-fortune/
    else
        echo "no tips"
    fi
}

printf "\n"
logo
fortune-classic; echo; fortune-tips
# pr -mts'	' <(logo | sed '/^$/d') <(fortune-classic; printf "\n"; fortune-tips)
printf "\n"
# }
