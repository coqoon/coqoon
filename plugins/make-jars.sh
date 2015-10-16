#!/bin/sh

# Copyright © 2013 Alexander Faithfull.
#
# This build script is free software; its author gives unlimited permission
# to copy and/or distribute it, with or without modifications, as long as this
# notice is preserved.

getmprop() {
	grep-dctrl --show-field="$1" --no-field-names "" META-INF/MANIFEST.MF
}

buildjar() {
	(
		cd "$1"
		shift 1

		name="`getmprop Bundle-Name`"
		id="`getmprop Bundle-SymbolicName`"; id=${id%%;*}
		version="`getmprop Bundle-Version`"
		echo "Building $version of $name ($id)"

		jar cfm "../${id}_$version.jar" META-INF/MANIFEST.MF "$@"
	)
}

buildjar dk.itu.coqoon.core \
	src/ lib/ plugin.xml schema/ license.txt -C bin/ dk/
buildjar dk.itu.coqoon.ui \
	src/ plugin.xml icons/ -C bin/ dk/
buildjar dk.itu.coqoon.ocaide \
	src/ lib/ plugin.xml -C bin/ dk/
