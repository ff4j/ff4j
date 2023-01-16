package org.ff4j.feature.exception;

import java.io.Serial;

/**
 * Store could be parameterized to through exception when Feature not found.
 */
public class FeatureNotFoundException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /** target message. */
    public static final String ERROR_MESSAGE_APP = "Feature '%s' does not exist in namespace '%s'";

    /**
     * Parameterized constructor.
     * 
     * @param namespace
     *           namespace name
     * @param uid
     *           feature identifier

     **/
    public FeatureNotFoundException(String namespace, String uid) {
        super(String.format(ERROR_MESSAGE_APP, uid, namespace));
    }
   
}
