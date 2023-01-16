package org.ff4j.security.exception;

import java.io.Serial;

/**
 * Thrown when a user access a resource with not enough privileges.
 */
public class AuthenticationException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /**
     * Resource access not authorized.
     *
     * @param username
     *      username
     */
    public AuthenticationException(String username) {
        super(String.format("Cannot authenticate user %s, Make sure user exists and provided correct password", username));
    }
    
}
