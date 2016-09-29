package org.ff4j.web.api.security;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.core.SecurityContext;

import org.ff4j.security.AbstractAuthorizationManager;
import org.glassfish.jersey.server.ContainerRequest;


/**
 * Implementation of Manager to base permissions on incoming user.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4JSecurityContextAuthenticationManager extends AbstractAuthorizationManager{

    /** {@inheritDoc} */
    public String getCurrentUserName() {
        SecurityContext wrapper = FF4JSecurityContextHolder.getSecurityContext();
        if (wrapper instanceof ContainerRequest) {
            SecurityContext sc = ((ContainerRequest) wrapper).getSecurityContext();
            if (sc != null && sc instanceof FF4jSecurityContext) {
                return ((FF4jSecurityContext) sc).getUserName();
            }
        }
        return null;
    }
    
    /** {@inheritDoc} */
    public Set<String> getCurrentUserPermissions() {
        SecurityContext wrapper = FF4JSecurityContextHolder.getSecurityContext();
        if (wrapper instanceof ContainerRequest) {
            SecurityContext sc = ((ContainerRequest) wrapper).getSecurityContext();
            if (sc != null && sc instanceof FF4jSecurityContext) {
                return ((FF4jSecurityContext) sc).getUserRoles();
            }
        }
        return new HashSet<String>();
    }

    /** {@inheritDoc} */
    public Set<String> listAllPermissions() {
        Set < String > vars = new HashSet<String>();
        if (FF4jAuthenticationFilter.getApiConfig() != null) {
            Map < String, Set<String > > perms = FF4jAuthenticationFilter.getApiConfig().getPermissions();
            for (Map.Entry<String,Set<String>> var : perms.entrySet()) {
                perms.get(var.getKey()).addAll(var.getValue());
            }
        }
        return vars;
    }
    
}
