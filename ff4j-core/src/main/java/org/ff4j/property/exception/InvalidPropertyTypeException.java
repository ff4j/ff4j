package org.ff4j.property.exception;

import java.io.Serial;

/**
 * Accessing invalid property.
 */
public class InvalidPropertyTypeException extends RuntimeException {

    /**
     * Serial.
     */
    @Serial
    private static final long serialVersionUID = 1L;

    /** Error Message. */
    public static final String ERROR_MESSAGE_APP = "Invalid property '%s' (workspace '%s'), expected %s but was %s";

    /**
     * Parameterized constructor.
     *
     * @param namespace
     *           namespace name
     * @param uid
     *           Property identifier
     * @param expected
     *          expected type
     * @param value
     *          real type
     **/
    public InvalidPropertyTypeException(String namespace, String uid, Class<?> expected, Class<?> value) {
        super(String.format(ERROR_MESSAGE_APP, uid, namespace, expected.getName(), value.getName()));
    }
}
