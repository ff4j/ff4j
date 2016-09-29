package org.ff4j.web.api.security;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;

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

import javax.annotation.security.DenyAll;
import javax.annotation.security.PermitAll;
import javax.annotation.security.RolesAllowed;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerRequestFilter;
import javax.ws.rs.container.ResourceInfo;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.Response.Status;

import org.ff4j.web.ApiConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Filter to get security.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jAuthorizationFilter implements ContainerRequestFilter {

    /** logger for this class. */
    private final Logger log = LoggerFactory.getLogger(getClass());
    
    /** security configuration. */
    private static ApiConfig apiConfig = null;
    
    @Context
    public ResourceInfo info;
    
    /**
     * Apply the filter ozz: check input request, validate or not with user auth
     * 
     * @param containerRequest The request from Tomcat server
     */
    @Override
    public void filter(ContainerRequestContext containerRequest) throws IOException {
        String path = containerRequest.getUriInfo().getPath();
        log.debug("Entering authorization filter for <" + path + ">");
        
        // @Denyall anywhere => none shall pass => Error 403
        if (isDenyAll()) forbidden();
        
        // THEN @PermitAll anywhere => everybodey pass
        if (isPermitAll()) return;
        
        // Check @RoleAllowed against SecurityContext
        if (isRolesAllowed()) {
            
            SecurityContext sc = containerRequest.getSecurityContext();
            if (sc instanceof FF4jSecurityContext) {
                Set < String > expectedRoles = getRoles();
                FF4jSecurityContext fsc = (FF4jSecurityContext) sc;
                Set <String> permissions = fsc.getUserRoles();
                if (permissions != null) {
                    for (String userPermission : permissions) {
                        if (expectedRoles.contains(userPermission)) {
                            return;
                        }
                    }
                }
                log.warn("Request Forbidden : user role are " + permissions + " but target expected=" + expectedRoles);
                forbidden();
            }
                
        }
        return;
    }
    
    private boolean isPermitAll() {
        return info.getResourceMethod().getAnnotation(PermitAll.class) != null;
    }
    
    private boolean isDenyAll() {
        return info.getResourceMethod().getAnnotation(DenyAll.class) != null;
    }
    
    private boolean isRolesAllowed() {
        return info.getResourceMethod().getAnnotation(RolesAllowed.class) != null;
    }
    
    private Set < String > getRoles() {
        Set < String > roles = new HashSet<String>();
        RolesAllowed ra1 = info.getResourceClass().getAnnotation(RolesAllowed.class);
        if (ra1 != null) {
            roles.addAll(Arrays.asList(ra1.value()));
        }
        RolesAllowed ra2 = info.getResourceMethod().getAnnotation(RolesAllowed.class);
        if (ra2 != null) {
            roles.addAll(Arrays.asList(ra2.value()));
        }
        return roles;
        
    }
    
    private static void forbidden() {
        throw new WebApplicationException(Response.status(Status.FORBIDDEN) //
                .entity("Cannot reach ressource, forbidden check @RoleAllowed, @DenyAll") //
                .type(MediaType.TEXT_HTML_TYPE).build());
    }

    /**
     * Getter accessor for attribute 'apiConfig'.
     *
     * @return current value of 'apiConfig'
     */
    public static ApiConfig getApiConfig() {
        return apiConfig;
    }

    /**
     * Setter accessor for attribute 'apiConfig'.
     * @param apiConfig new value for 'apiConfig'
     */
    public static void setApiConfig(ApiConfig apiConfig) {
        FF4jAuthorizationFilter.apiConfig = apiConfig;
    }

    /**
     * Getter accessor for attribute 'info'.
     *
     * @return current value of 'info'
     */
    public ResourceInfo getInfo() {
        return info;
    }

    /**
     * Setter accessor for attribute 'info'.
     * @param info new value for 'info '
     */
    public void setInfo(ResourceInfo info) {
        this.info = info;
    }

}
