package org.ff4j.feature.exception;

import java.io.Serial;

/**
 * Store could be parameterized to through exception when Feature not found.
 */
public class FeatureFlagAlreadyExistException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /** Error Message. */
    public static final String ERROR_MESSAGE_APP = "Feature '%s' already exist in namespace '%s'";

    /**
     * Parameterized constructor.
     *
     * @param namespace
     *           namespace name
     * @param uid
     *           feature identifier
     **/
    public FeatureFlagAlreadyExistException(String namespace, String uid) {
        super(String.format(ERROR_MESSAGE_APP, uid, namespace));
    }
   
}
