package org.ff4j.namespace.exception;

import java.io.Serial;

/**
 * Specialized exception when namespace already exists.
 */
public class NamespaceAlreadyException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /** target message. */
    public static final String ERROR_MESSAGE_APP = "Namespace '%s' already exist.";

    /**
     * Parameterized constructor.
     *
     * @param namespace
     *           namespace name
     **/
    public NamespaceAlreadyException(String namespace) {
        super(String.format(ERROR_MESSAGE_APP, namespace));
    }
   
}
