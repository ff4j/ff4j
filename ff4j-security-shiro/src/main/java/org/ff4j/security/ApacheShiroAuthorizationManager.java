package org.ff4j.security;

/*-
 * #%L
 * ff4j-security-shiro
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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
import java.util.Map;
import java.util.Set;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.subject.PrincipalCollection;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.CollectionUtils;
import org.ff4j.utils.Util;

/**
 * Leverage on Shiro.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class ApacheShiroAuthorizationManager extends AbstractAuthorizationManager {
    
    /** Key to read. **/
    private static final String PRINCIPAL_USERNAME = "username";
    
    /** {@inheritDoc} */
    public boolean isAllowed(Set<String> permissions) {
        boolean[] arrayOfChecks     = SecurityUtils
                .getSubject()
                .isPermitted(permissions.toArray(new String[0]));
        for(int i=0;i<arrayOfChecks.length;i++) {
            if (arrayOfChecks[i]) return true;
        }
        return false;
    }
    
    /** {@inheritDoc} */
    @Override
    @SuppressWarnings("rawtypes")
    public String getCurrentUserName() {
        String username = "N/A";
        Subject subject = SecurityUtils.getSubject();
        PrincipalCollection principalCollection = subject.getPrincipals();
        if (principalCollection != null && !principalCollection.isEmpty()) {
            Collection<Map> principalMaps = subject.getPrincipals().byType(Map.class);
            if (CollectionUtils.isEmpty(principalMaps)) {
                username = subject.getPrincipal().toString();
            }
            else {
                username = (String) principalMaps.iterator().next().get(PRINCIPAL_USERNAME);
            }
        }
        return username;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> getCurrentUserPermissions() {
        return Util.set("Not supported");
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listAllPermissions() {
        return Util.set("Not supported");
    }

    
}
