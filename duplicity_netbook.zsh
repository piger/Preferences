#!/usr/bin/env zsh

# Backup per la home del netbook aziendale.

duplicity \
	--exclude /home/sand/.toast \
	--exclude /home/sand/.cache \
	--exclude /home/sand/.thumbnails \
	--exclude /home/sand/.ccache \
	--exclude "/home/sand/VirtualBox VMs" \
	--exclude /home/sand/dev/pyvuln/env.old \
	--exclude "/home/sand/dev/**/env/" \
	--exclude "/home/sand/**/*.pyc" \
	--exclude /home/sand/tools/msf3 \
	--exclude /home/sand/tools/msf3_3.5.2 \
	--exclude /home/sand/Downloads \
	--name home_daily \
	--encrypt-key D3B75D54 \
	--sign-key D3B75D54 \
	-v 6 \
	--gpg-options "--compress-algo=bzip2" \
	/home/sand \
	scp://sand@taleggio/backup/hal/
