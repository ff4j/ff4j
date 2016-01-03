package org.ff4j.test;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

import org.ff4j.security.AbstractAuthorizationManager;

public class DefinedPermissionSecurityManager extends AbstractAuthorizationManager {
    
    private Set<String> internal = null;
    
    public DefinedPermissionSecurityManager(Set < String > value) {
        this.internal = value;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> getCurrentUserPermissions() {
        return internal;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listAllPermissions() {
        return getCurrentUserPermissions();
    }

}
