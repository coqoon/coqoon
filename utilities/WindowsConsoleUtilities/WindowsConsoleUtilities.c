#define WIN32_LEAN_AND_MEAN
#define _WIN32_WINNT 0x0501

#include "dk_itu_coqoon_core_coqtop_WindowsConsoleUtilities_Native.h"

#include <windows.h>

/*	This file is free software; its author gives unlimited permission to
	copy and/or distribute it, with or without modifications. */

JNIEXPORT void JNICALL
Java_dk_itu_coqoon_core_coqtop_WindowsConsoleUtilities_00024Native_sendCtrlC(
		JNIEnv* env,
		jclass unused) {
	GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
}

JNIEXPORT void JNICALL
Java_dk_itu_coqoon_core_coqtop_WindowsConsoleUtilities_00024Native_ignoreCtrlC(
		JNIEnv* env,
		jclass unused) {
	SetConsoleCtrlHandler(NULL, TRUE);
}

JNIEXPORT void JNICALL
Java_dk_itu_coqoon_core_coqtop_WindowsConsoleUtilities_00024Native_unignoreCtrlC(
		JNIEnv* env,
		jclass unused) {
  	SetConsoleCtrlHandler(NULL, FALSE);
}

JNIEXPORT void JNICALL
Java_dk_itu_coqoon_core_coqtop_WindowsConsoleUtilities_00024Native_freeConsole(
		JNIEnv* env,
		jclass unused) {
  	FreeConsole();
}

JNIEXPORT void JNICALL
Java_dk_itu_coqoon_core_coqtop_WindowsConsoleUtilities_00024Native_attachConsole(
		JNIEnv* env,
		jclass unused,
		jlong pid) {
	AttachConsole((DWORD)pid);
}

JNIEXPORT jlong JNICALL
Java_dk_itu_coqoon_core_coqtop_WindowsConsoleUtilities_00024Native_getProcessId(
		JNIEnv* env,
		jclass unused,
		jlong handle) {
	HANDLE h = (HANDLE)handle;
	return GetProcessId(h);
}

