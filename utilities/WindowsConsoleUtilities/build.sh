#!/bin/sh

# This file is free software; its author gives unlimited permission to copy
# and/or distribute it, with or without modifications.

# To be able to run this script, you'll need to have the MinGW-w64 development
# environment installed (available in the Debian package "mingw-w64"), and to
# adjust the "JDK_BASE" variable below to point to the JDK on your computer.

ead() {
	echo "$@" && "$@"
}

export "JDK_BASE=/home/alec/Downloads/jdk-1fe539ed00bc"

set -e

ead rm -f "dk_itu_coqoon_core_coqtop_WindowsConsoleUtilities_Native.h"
ead javah \
	-classpath ../../plugins/dk.itu.coqoon.core/bin \
	dk.itu.coqoon.core.coqtop.WindowsConsoleUtilities.Native

for i in i686 x86_64; do
	ead mkdir -p "lib/$i"
	ead ${i}-w64-mingw32-gcc \
		-D_JNI_IMPLEMENTATION_ -Wl,--kill-at \
		"-I$JDK_BASE/src/share/javavm/export" \
		"-I$JDK_BASE/src/windows/javavm/export" \
		-shared -o "lib/$i/WindowsConsoleUtilities_Native.dll" \
		WindowsConsoleUtilities.c
done
