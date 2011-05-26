#!/bin/zsh
#

#rsync -avz --progress --delete --exclude .Spotlight-V100/ \
#    --exclude .dropbox/ --exclude Downloads/ \
#    --exclude Dropbox/ --exclude Library/ \
#    --exclude .Trashes/ --exclude .fseventsd/ \
#    ~sand/ sand@taleggio:backup/osx/home_sand

rdiff-backup --exclude-device-files --exclude-sockets \
    --preserve-numerical-ids --print-statistics \
    -v 5 \
    --exclude ~/.Spotlight-V100/ \
    --exclude ~/.dropbox/ \
    --exclude ~/Downloads/ \
    --exclude ~/Dropbox/ \
    --exclude ~/Library/ \
    --exclude ~/.Trashes/ \
    --exclude ~/.fseventsd/ \
    ~sand/ sand@taleggio::backup/osx/home_sand
