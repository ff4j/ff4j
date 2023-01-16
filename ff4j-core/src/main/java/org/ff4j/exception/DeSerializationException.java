package org.ff4j.exception;

import java.io.Serial;

/**
 * Error when deserializing object from String.
 */
public class DeSerializationException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /**
     * Parameterized constructor.
     *
     * @param expression
     *            Exception message
     * @param myType
     *            target type
     **/
    public DeSerializationException(String expression, Class<?> myType) {
        super(String.format("Cannot deserialize '%s' to expected '%s'",
                expression, myType.getName()));
    }

    /**
     * Parameterized constructor.
     *
     * @param expression
     *            Exception message
     * @param myType
     *            target type
     * @param parent
     *            parent exception
     **/
    public DeSerializationException(String expression, Class<?> myType, Throwable parent) {
       super(String.format("Cannot deserialize '%s' to expected '%s'",
                expression, myType.getName()), parent);
    }

}
