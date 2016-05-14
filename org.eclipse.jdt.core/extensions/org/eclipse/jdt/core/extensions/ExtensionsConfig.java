package org.eclipse.jdt.core.extensions;

/**
 * Configuration flags
 */
public class ExtensionsConfig {
	// Enables the extensions (and allows to easily find all the hooks in the JDT code)
	public static final boolean ENABLE = true;
	
	// Direct logging of some debug messages to the error stream (not proper Eclipse logging)
	public static final boolean ENABLE_LOGS = false;

	public static void log(String message) {
		if (!ENABLE_LOGS) return;
		System.err.println(message);
		System.err.flush();
	}

	public static String asLog(Object object) {
		if (!ENABLE_LOGS) return null;
		String objString = object.toString();
		if (objString.length() > 200)
			objString = objString.substring(0, 200);
		objString = objString.replace("\n", "\\n"); //$NON-NLS-1$ //$NON-NLS-2$
		return objString + " [" + System.identityHashCode(object) + "]"; //$NON-NLS-1$ //$NON-NLS-2$
	}
}
