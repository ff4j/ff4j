package org.ff4j.security;

import org.ff4j.user.FF4jUser;

/**
 * Login against target user provider and provider
 * user with meta data.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public interface AuthenticationManager {
    
    /**
     * Authentication and retrieve user Information.
     *
     * @param userName
     *          current userName
     * @param password
     *          current password
     * @return
     *          user information
     */
    FF4jUser authenticate(String userName, String password);
    
}
