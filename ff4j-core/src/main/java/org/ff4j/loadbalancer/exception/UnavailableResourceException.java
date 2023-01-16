package org.ff4j.loadbalancer.exception;

import java.io.Serial;

/**
 * To disable a resources from the load balancer you will get 2 scenarios:
 * - The heart bit detect the failure and disable the resource
 * - The usage of the resource generates an exception that can be interpreted as resource not available
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class UnavailableResourceException extends RuntimeException {

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
    public UnavailableResourceException(String msg) {
        super(msg);
    }
    
    /**
     * Error with message and error.
     *
     * @param msg
     *      current message
     * @param parent
     *      error
     */
    public UnavailableResourceException(String msg, Throwable parent) {
        super(msg, parent);
    }

}
