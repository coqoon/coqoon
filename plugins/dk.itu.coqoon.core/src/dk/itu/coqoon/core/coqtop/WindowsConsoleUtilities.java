package dk.itu.coqoon.core.coqtop;

class WindowsConsoleUtilities {
	private static class Native {
		static {
			System.loadLibrary("WindowsConsoleUtilities_Native");
		}

		private static native void sendCtrlC();
		private static native void ignoreCtrlC();
		private static native void unignoreCtrlC();

		private static native void freeConsole();
		private static native void attachConsole(long pid);

		private static native long getProcessId(long handle);
	}

	public static void sendCtrlC() {
		Native.sendCtrlC();
	}
	public static void ignoreCtrlC() {
		Native.ignoreCtrlC();
	}
	public static void unignoreCtrlC() {
		Native.unignoreCtrlC();
	}

	public static void freeConsole() {
		Native.freeConsole();
	}
	public static void attachConsole(long pid) {
		Native.attachConsole(pid);
	}

	public static long getProcessId(long handle) {
		return Native.getProcessId(handle);
	}
}
