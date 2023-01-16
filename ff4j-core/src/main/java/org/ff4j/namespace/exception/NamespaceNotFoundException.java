package org.ff4j.namespace.exception;

import java.io.Serial;

/**
 * Specialized exception when namespace is not found.
 */
public class NamespaceNotFoundException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /** target message. */
    public static final String ERROR_MESSAGE_APP = "Namespace '%s' does not exist.";

    /**
     * Parameterized constructor.
     *
     * @param namespace
     *           namespace name
     **/
    public NamespaceNotFoundException(String namespace) {
        super(String.format(ERROR_MESSAGE_APP, namespace));
    }
   
}
