package org.ff4j.security;

import org.ff4j.security.exception.AuthenticationException;

import java.util.Set;

/**
 * Authentication to the system.
 */
public interface AuthenticationManager {

    /** Permission Name.*/
    String FF4J_PERMISSION_CONFIGURATION_EDIT = "FF4J_CONFIGURATION_EDIT";

    /** Permission Name.*/
    String FF4J_PERMISSION_CREATE_SCHEMA      = "FF4J_CREATE_SCHEMA";

    /**
     * Authentication.
     * @param username
     *      current username
     * @param password
     *      current password
     * @throws AuthenticationException
     *      invalid credentials or cannot authenticate.
     * @return
     *      session token
     */
    FF4jUser authenticate(String username, String password)
    throws AuthenticationException;

}
