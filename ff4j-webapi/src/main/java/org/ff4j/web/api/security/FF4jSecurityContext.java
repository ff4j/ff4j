package org.ff4j.web.api.security;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import java.io.Serializable;
import java.security.Principal;
import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.core.SecurityContext;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Default implementation of security context.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jSecurityContext implements SecurityContext, Serializable {

    public static final Logger logger = LoggerFactory.getLogger(FF4jSecurityContext.class);

    /** Serial. */
    private static final long serialVersionUID = 9041009506390024931L;

    /** login using user/password. */
    public static final String AUTH_SCHEME_BASIC = "BASIC";
    
    /** login using apiKey. */
    public static final String AUTH_SCHEME_APIKEY = "APIKEY";
    
    /** Permissions for user. */
    private Set<String> userRoles = new HashSet<String>();
    
    /** Authorieation scheme. */
    private String authScheme;
    
    /** Current authenticated username. */
    private final String userName;
    
    /**
     * Custom security context.
     *
     * @param userName
     *            target username
     * @param authScheme
     *            target authScheme
     * @param perm
     *            target permission
     */
    public FF4jSecurityContext(String userName, String authSchem, Set < String > perm) {
        this.userName   = userName;
        this.userRoles  = perm;
        this.authScheme = authSchem;
    }

    /** {@inheritDoc} */
    @Override
    public Principal getUserPrincipal() {
        logger.info("PRINCP");
        return new Principal() {
            /** {@inheritDoc} */
            @Override
            public String getName() {
                return userName;
            }
        };
    }

    /** {@inheritDoc} */
    @Override
    public boolean isUserInRole(String role) {
        logger.info("TEST ROLE " + role + " against " + userRoles);
        return userRoles.contains(role);
    }

    /** {@inheritDoc} */
    @Override
    public boolean isSecure() {
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public String getAuthenticationScheme() {
        return authScheme;
    }

    /**
     * Getter accessor for attribute 'userRoles'.
     *
     * @return
     *       current value of 'userRoles'
     */
    public Set<String> getUserRoles() {
        return userRoles;
    }

    /**
     * Setter accessor for attribute 'userRoles'.
     * @param userRoles
     * 		new value for 'userRoles '
     */
    public void setUserRoles(Set<String> userRoles) {
        this.userRoles = userRoles;
    }

    /**
     * Getter accessor for attribute 'userName'.
     *
     * @return
     *       current value of 'userName'
     */
    public String getUserName() {
        return userName;
    }

}
