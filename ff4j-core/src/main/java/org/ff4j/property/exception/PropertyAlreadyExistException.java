package org.ff4j.property.exception;

import java.io.Serial;

/**
 * Store could be parameterized to through exception when Property not found.
 */
public class PropertyAlreadyExistException extends RuntimeException {
	
	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
	
    /** Error Message. */
    public static final String ERROR_MESSAGE_APP = "Property '%s' already exist in workspace '%s'";

    /**
     * Parameterized constructor.
     *
     * @param workspace
     *           workspace name
     * @param uid
     *           Property identifier
     **/
    public PropertyAlreadyExistException(String workspace, String uid) {
        super(String.format(ERROR_MESSAGE_APP, uid, workspace));
    }
   
}
