package org.ff4j.exception;

import java.io.Serial;

/**
 * Specialized error when validation failed.
 */
public class AssertionViolationException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;

	/**
     * Parameterized constructor.
     * 
     * @param message
     *            Exception message
     **/
    public AssertionViolationException(String message) {
        super(message);
    }

    /**
     * Parameterized constructor.
     * 
     * @param message
     *          Exception message
     * @param parent
     *          parent exception
     **/
    public AssertionViolationException(String message, Throwable parent) {
        super(message, parent);
    }

}
