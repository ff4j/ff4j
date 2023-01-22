package org.ff4j.feature.exception;

import java.io.Serial;

/**
 * Store could be parameterized to through exception when Feature not found.
 */
public class FeatureFlagNotFoundException extends RuntimeException {

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
     * @param workspace
     *           workspace name
     * @param uid
     *           feature identifier

     **/
    public FeatureFlagNotFoundException(String workspace, String uid) {
        super(String.format(ERROR_MESSAGE_APP, uid, workspace));
    }
   
}
