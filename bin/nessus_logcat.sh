#!/bin/sh
# Greppa per:
# [Wed Apr  6 13:05:40 2011][15850.34851] Finished testing localhost.localdomain. Time : 2638.04 secs

tail -f /opt/nessus/var/nessus/logs/nessusd.messages | egrep "(: testing|Finished )"
