package org.ff4j.security.exception;

import java.io.Serial;

/**
 * Thrown when a user access a resource with not enough privileges.
 */
public class NotAuthorizedException extends RuntimeException {

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
     * @param permission
     *      permission
     */
    public NotAuthorizedException(String username, String permission) {
        super(String.format("User '%s' has not permission/role '%s' to execute this action.", username, permission));
    }

}
