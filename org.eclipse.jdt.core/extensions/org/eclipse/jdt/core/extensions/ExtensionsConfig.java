package org.eclipse.jdt.core.extensions;

/**
 * Configuration flags
 */
public class ExtensionsConfig {
	// Enables the extensions (and allows to easily find all the hooks in the JDT code)
	public static final boolean Enable = true;
	
	// Direct logging of some debug messages to the error stream (not proper Eclipse logging)
	public static final boolean EnableLogs = false;

	public static void log(String message) {
		if (!EnableLogs) return;
		System.err.println(message);
		System.err.flush();
	}

	public static String asLog(Object object) {
		if (!EnableLogs) return null;
		String objString = object.toString();
		if (objString.length() > 200)
			objString = objString.substring(0, 200);
		objString = objString.replace("\n", "\\n"); //$NON-NLS-1$ //$NON-NLS-2$
		return objString + " [" + System.identityHashCode(object) + "]"; //$NON-NLS-1$ //$NON-NLS-2$
	}
}
