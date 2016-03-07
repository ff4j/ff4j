package org.ff4j.test.security;

/*
 * #%L
 * ff4j-test
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


import java.util.HashSet;
import java.util.Set;

import org.ff4j.security.AuthorizationsManager;

/**
 * Helper to test {@link DefaultAuthorisationManager} interface.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class DefaultAuthorisationManager implements AuthorizationsManager {

    /** Current userName. */
    private String userName;
    
    /** All definitions. */
    private  Set<String> userDef = new HashSet<String>();
    
    /** All definitions. */
    private Set < String > allDef = new HashSet<String>();
    
    /**
     * Constructor for manager.
     */
    public DefaultAuthorisationManager() {
    }
    
    /**
     * Constructor for manager.
     * @param user
     *      user permissions
     * @param all
     *      all permissions.
     */
    public DefaultAuthorisationManager(Set<String> user, Set<String> all) {
        this.userDef    = user;
        this.allDef     = all;
    }
    

    /**
     * Constructor for manager.
     *
     * @param user
     *      user permissions
     * @param all
     *      all permissions.
     */
    public DefaultAuthorisationManager(Set<String> user, Set<String> all, String userName) {
        this(user, all);
        this.userName = userName;
    }
    
    /** {@inheritDoc} */
    public String getCurrentUserName() {
        return userName;
    }
    
    /** {@inheritDoc} */
    public Set<String> getCurrentUserPermissions() {
        return userDef;
    }
    
    /** {@inheritDoc} */
    public void setCurrentUserPermissions(Set<String> user) {
        this.userDef = user;
    }

    /** {@inheritDoc} */
    public Set<String> listAllPermissions() {
        return allDef;
    }
    
    /** {@inheritDoc} */
    public void setAllPermissions(Set<String> all) {
        this.allDef = all;
    }

    /** {@inheritDoc} */
    public String toJson() {
        return DefaultAuthorisationManager.class.getName();
    }

    

}
