package org.ff4j.exception;

import java.io.Serial;

/**
 * Error when serializing object to String.
 */
public class SerializationException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /**
     * Parameterized constructor.
     *
     * @param value
     *            value
     **/
    public <T> SerializationException(Object value) {
        super(String.format("Cannot serialize '%s' of type '%s' to String", value, value.getClass().getName()));
    }

    /**
     * Parameterized constructor.
     *
     * @param value
     *            value
     * @param parent
     *            parent exception
     **/
    public SerializationException(Object value, Throwable parent) {
        super(String.format("Cannot serialize '%s' of type '%s' to String", value, value.getClass().getName()), parent);
    }

}
