package org.ff4j.exception;

import java.io.Serial;

/**
 * Exception with type manipulation.
 */
public class InvalidEvaluationPolicyException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /**
     * Parameterized constructor.
     * 
     * @param type
     *            Exception type
     **/
    public InvalidEvaluationPolicyException(String type) {
        super(msg(type));
    }

    /**
     * Parameterized constructor.
     * 
     * @param type
     *            Exception type
     * @param parent
     *            parent exception
     * 
     **/
    public InvalidEvaluationPolicyException(String type, Throwable parent) {
        super(msg(type), parent);
    }

    /**
     * build error message.
     *
     * @param type
     *      type conversion
     * @return
     *      nessage conversion
     */
    private static String msg(String type) {
        return String.format("Cannot initialize %s type, check syntax and constructors", type);
    }


}
