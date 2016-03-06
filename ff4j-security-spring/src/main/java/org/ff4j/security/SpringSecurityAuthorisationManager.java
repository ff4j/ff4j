package org.ff4j.security;

/*
 * #%L SpringSecurityAuthorisationManager.java (ff4j-security-spring) by Cedrick LUNVEN %% Copyright (C) 2013 Ff4J %% Licensed
 * under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may
 * obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.LinkedHashSet;
import java.util.Set;

import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

/**
 * Implementation of {@link AuthorizationsManager} with SpringSecurity framework.
 * 
 * @author clunven
 * 
 */
public class SpringSecurityAuthorisationManager extends AbstractAuthorizationManager {

    /** {@inheritDoc} */
    public Set<String> getCurrentUserPermissions() {
        Set<String> listOfRoles = new LinkedHashSet<String>();
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (!(auth instanceof AnonymousAuthenticationToken)) {
            for (GrantedAuthority grantedAuthority : auth.getAuthorities()) {
                listOfRoles.add(grantedAuthority.getAuthority());
            }
        }
        return listOfRoles;
    }

    /** {@inheritDoc} */
    public Set<String> listAllPermissions() {
        return getCurrentUserPermissions();
    }

    /** {@inheritDoc} */
    public String getCurrentUserName() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (!(auth instanceof AnonymousAuthenticationToken)) {
            return auth.getName();
        }
        return "anonymous";
    }   

}