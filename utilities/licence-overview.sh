#!/bin/sh

apachel=""
otherl=""

for i in `find ../plugins/ -iname "*.scala"`; do
	if grep --silent "Apache License" "$i"; then
		apachel="$apachel $i"
	else
		otherl="$otherl $i"
	fi
done

COLUMNS="${COLUMNS:-`tput cols`}"

echo "Apache License" > apl.out
wc -l $apachel >> apl.out
echo "Other" > otl.out
wc -l $otherl >> otl.out
pr --merge --omit-header --omit-pagination "--width=$COLUMNS" apl.out otl.out
rm apl.out otl.out
