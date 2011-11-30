#!/usr/bin/env zsh
# osx:
# $7 = bytes in, $8 = bytes out

if [[ $OSTYPE == darwin* ]]; then
	netstat -b -i -I en1 | awk 'NR > 1 { print $7 }' | sort -nu | head -n 1 >> /tmp/meter.txt
else
	ifconfig eth0 | grep "RX bytes" | awk '{ print $2 }' | cut -d: -f2 >> /tmp/meter.txt
fi

lines=$(tail -n 10 /tmp/meter.txt)
echo "$lines" > /tmp/meter.txt

{
	for i in {1..9}; do
			(( ii = i + 1 ))
			a=${${(f)lines}[$i]}
			b=${${(f)lines}[$ii]}
			echo -n $(echo "$b - $a" | bc)
			(( ii < 10 )) && echo -n "," || echo
	done
} | ~/dev/others/spark/spark
