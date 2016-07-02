package org.eclipse.jdt.core.extensions;

/**
 * Configuration flags
 */
public class ExtensionsConfig {
	// Enables the extensions (and allows to easily find all the hooks in the JDT code)
	public static final boolean Enable = true;
	
	// Direct logging of some debug messages to the error stream (not proper Eclipse logging)
	public static final boolean Debug = false;
	
	public static final boolean ReverseExtensions = false;
	
	public static int StaticImportRelevance = 30; // prioritize completion from static import
	public static int DebugCompletionTimeout = 900000; // for debugging the completion engine

	public static void log(String message) {
		if (!Debug) return;
		System.err.println(message);
		System.err.flush();
	}

	public static String asLog(Object object) {
		if (!Debug) return null;
		String objString = object.toString();
		if (objString.length() > 200)
			objString = objString.substring(0, 200);
		objString = objString.replace("\n", "\\n"); //$NON-NLS-1$ //$NON-NLS-2$
		return objString + " [" + System.identityHashCode(object) + "]"; //$NON-NLS-1$ //$NON-NLS-2$
	}
}
