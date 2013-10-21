package org.ff4j.security;

/*
 * #%L
 * SpringSecurityAuthorisationManager.java (ff4j-security-spring) by Cedrick LUNVEN
 * %%
 * Copyright (C) 2013 Ff4J
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

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
public class SpringSecurityAuthorisationManager implements AuthorizationsManager {

    /** Logger for Advisor. */
    static final Logger LOG = LoggerFactory.getLogger(SpringSecurityAuthorisationManager.class);

    /**
     * Log Once the warning
     */
    public SpringSecurityAuthorisationManager() {
        LOG.warn("Only user roles are loaded - cannot retrieve roles of EVERYONE as a list");
    }

    @Override
    public Set<String> getAuthenticatedUserRoles() {
        Set<String> listOfRoles = new LinkedHashSet<String>();
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (!(auth instanceof AnonymousAuthenticationToken)) {
            Collection<GrantedAuthority> rights = auth.getAuthorities();
            for (GrantedAuthority grantedAuthority : rights) {
                listOfRoles.add(grantedAuthority.getAuthority());
            }
        }
        return listOfRoles;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> getEveryOneRoles() {
        return getAuthenticatedUserRoles();
    }

}