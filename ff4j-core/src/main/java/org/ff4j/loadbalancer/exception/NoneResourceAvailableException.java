package org.ff4j.loadbalancer.exception;

import java.io.Serial;

/**
 * Error when no resources are enabled anymore
 */
public class NoneResourceAvailableException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /**
     * Error with message
     *
     * @param msg
     *      current message
     */
    public NoneResourceAvailableException(String msg) {
        super(msg);
    }

}
