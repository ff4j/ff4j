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

import java.util.Set;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.SecurityContext;

import org.ff4j.web.ApiConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.jersey.core.util.Base64;
import com.sun.jersey.spi.container.ContainerRequest;
import com.sun.jersey.spi.container.ContainerRequestFilter;
import com.sun.jersey.spi.container.ContainerResponseFilter;
import com.sun.jersey.spi.container.ResourceFilter;

import static org.ff4j.web.FF4jWebConstants.*;

/**
 * Filter to get security.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jSecurityContextFilter implements ContainerRequestFilter, ResourceFilter {

    /** logger for this class. */
    private final Logger log = LoggerFactory.getLogger(getClass());
    
    /** security configuration. */
    private static ApiConfig securityConfig = null;

    /**
     * Apply the filter : check input request, validate or not with user auth
     * 
     * @param containerRequest
     *            The request from Tomcat server
     */
    @Override
    public ContainerRequest filter(ContainerRequest containerRequest) throws WebApplicationException {
        String method = containerRequest.getMethod();
        String path = containerRequest.getPath(true);
        log.debug("Entering security filter for <" + path + ">");
 
        //We do allow wadl to be retrieve
        if (method.equals("GET") && (path.equals("application.wadl") || path.equals("application.wadl/xsd0.xsd"))) {
            log.info("Accessing schema and wadl ok");
            return containerRequest;
        }
 
        // Get the authentification passed in HTTP headers parameters
        String auth = containerRequest.getHeaderValue(HEADER_AUTHORIZATION);

        if (auth == null) {
            handleUnAuthorized("<p>'authorization' parameter is required in header  for authentication (HTTP-Basic or ApiKey)</p>");
        }

        // Identification of an Application with its api key
        if (auth.contains(PARAM_AUTHKEY)) {
            auth = auth.replaceFirst(PARAM_AUTHKEY + "=", "");
            // Checking api Key
            if (!securityConfig.getApiKeys().contains(auth)) {
                handleUnAuthorized("The api key provided '" + auth + "' is invalid ");
            }

            // Positionning Roles
            Set<String> perms = securityConfig.getPermissions().get(auth);
            SecurityContext sc = new FF4jSecurityContext(auth, PARAM_AUTHKEY, perms);
            containerRequest.setSecurityContext(sc);
            log.info("Client successfully logged with an ApiKey");
            return containerRequest;
        }

        // Identification of a final user in HTTP-BASIC MODE
        if (auth.toUpperCase().contains("BASIC")) {
            byte[] decodedBytes = Base64.decode(auth.replaceFirst("[B|b]asic ", ""));
            String[] lap = new String(decodedBytes).split(":", 2);
            if (lap == null || lap.length != 2) {
                handleUnAuthorized("Invalid BASIC Token, cannot parse");
            }

            // Validation login/password
            String expectedPassword = securityConfig.getUsers().get(lap[0]);
            if (expectedPassword == null || !(lap[1].equals(expectedPassword))) {
                handleUnAuthorized("<p>Invalid username or password.</p>");
            }
            
            // Positionning Roles
            Set<String> perms = securityConfig.getPermissions().get(lap[0]);
            SecurityContext sc = new FF4jSecurityContext(lap[0], "BASIC", perms);
            containerRequest.setSecurityContext(sc);
            log.info("Client successfully logged with a user/pasword pair ");
            return containerRequest;
        }

        handleUnAuthorized("Cannot parse authorisation header attribute, valid are basic and apiKey");
        return null;
    }

    /**
     * Dedicated error.
     * 
     * @param message
     *            target message
     */
    private void handleUnAuthorized(String message) {
        StringBuilder msg = new StringBuilder("<p style=\"color:#880000\">");
        msg.append("<H1>ERROR HTTP 401 : Unauthorized</H1>");
        msg.append("<p>" + message + "</p>");
        log.error("Authentication error :" + message);
        throw new WebApplicationException(Response.status(Status.UNAUTHORIZED).entity(msg.toString())
                .type(MediaType.TEXT_HTML_TYPE).build());
    }

    /** {@inheritDoc} */
    @Override
    public ContainerRequestFilter getRequestFilter() {
        return this;
    }

    /** {@inheritDoc} */
    @Override
    public ContainerResponseFilter getResponseFilter() {
        // No response filter
        return null;
    }

    public static ApiConfig getSecurityConfig() {
        return securityConfig;
    }

    public static void setSecurityConfig(ApiConfig securityConfig) {
        FF4jSecurityContextFilter.securityConfig = securityConfig;
    }
}
