package org.ff4j.exception;

import java.io.Serial;

/**
 * Store could be parameterized to through exception when Feature not found.
 */
public class ObjectNotFoundException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /** target message. */
    public static final String ERROR_MESSAGE_APP = "Object '%s' has not been found";

    /**
     * Parameterized constructor.
     *
     * @param uid
     *           object identifier

     **/
    public ObjectNotFoundException(String uid) {
        super(String.format(ERROR_MESSAGE_APP, uid, uid));
    }
   
}
