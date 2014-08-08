package org.ff4j.web.api.jersey;

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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.FF4j;
import org.ff4j.web.api.FF4JWebProvider;
import org.ff4j.web.api.FF4jWebConstants;

/**
 * Bean to configure security for the WebAPI. This custom bean is defined to limit dependencies to Spring security for instance.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jApiConfig implements FF4JWebProvider, FF4jWebConstants {

    /** Configuration of ff4j. */
    private FF4j fF4j;

    /** Enable authentication for jersey. */
    private boolean enableAuthentication = false;
    
    /** Enable authorization for jersey. */
    private boolean enableAuthorization = false;
    
    /** Enable loggin filter for jersey. */
    private boolean enableLogging = false;
    
    /** User of the API (login/password). */
    private Map<String, String> users = new HashMap<String, String>();

    /** will hold granted api Keys. */
    private Set<String> apiKeys = new HashSet<String>();

    /** will hold all permissions for both user and apiKey. */
    private Map<String, Set<String>> permissions = new HashMap<String, Set<String>>();

    /**
     * Default constructor.
     */
    public FF4jApiConfig() {
    }

    /**
     * Initialized with a ff4j.
     */
    public FF4jApiConfig(FF4j ff) {
        this.fF4j = ff;
    }

    /**
     * Helper method to create a user.
     * 
     * @param userName
     *            target username
     * @param password
     *            target password
     * @param read
     *            role read
     * @param write
     *            role write
     */
    public void createUser(String userName, String password, boolean read, boolean write) {
        users.put(userName, password);
        Set<String> tmpPerm = new HashSet<String>();
        if (read) {
            tmpPerm.add(ROLE_READ);
        }
        if (write) {
            tmpPerm.add(ROLE_WRITE);
        }
        permissions.put(userName, tmpPerm);
    }

    /**
     * Helper method to create an apiKey.
     * 
     * @param apiKey
     *            target apiKey
     * @param read
     *            role read
     * @param write
     *            role write
     */
    public void createApiKey(String apiKey, boolean read, boolean write) {
        apiKeys.add(apiKey);
        Set<String> tmpPerm = new HashSet<String>();
        if (read) {
            tmpPerm.add(ROLE_READ);
        }
        if (write) {
            tmpPerm.add(ROLE_WRITE);
        }
        permissions.put(apiKey, tmpPerm);
    }

    /** {@inheritDoc} */
    @Override
    public FF4j getFF4j() {
        return fF4j;
    }

    /**
     * Setter accessor for attribute 'fF4j'.
     * 
     * @param fF4j
     *            new value for 'fF4j '
     */
    public void setFF4j(FF4j fF4j) {
        this.fF4j = fF4j;
    }

    /**
     * Getter accessor for attribute 'enableAuthentication'.
     *
     * @return current value of 'enableAuthentication'
     */
    public boolean isEnableAuthentication() {
        return enableAuthentication;
    }

    /**
     * Setter accessor for attribute 'enableAuthentication'.
     * 
     * @param enableAuthentication
     *            new value for 'enableAuthentication '
     */
    public void setEnableAuthentication(boolean enableAuthentication) {
        this.enableAuthentication = enableAuthentication;
    }

    /**
     * Getter accessor for attribute 'enableAuthorization'.
     *
     * @return current value of 'enableAuthorization'
     */
    public boolean isEnableAuthorization() {
        return enableAuthorization;
    }

    /**
     * Setter accessor for attribute 'enableAuthorization'.
     * 
     * @param enableAuthorization
     *            new value for 'enableAuthorization '
     */
    public void setEnableAuthorization(boolean enableAuthorization) {
        this.enableAuthorization = enableAuthorization;
    }

    /**
     * Getter accessor for attribute 'enableLogging'.
     *
     * @return current value of 'enableLogging'
     */
    public boolean isEnableLogging() {
        return enableLogging;
    }

    /**
     * Setter accessor for attribute 'enableLogging'.
     * 
     * @param enableLogging
     *            new value for 'enableLogging '
     */
    public void setEnableLogging(boolean enableLogging) {
        this.enableLogging = enableLogging;
    }

    /**
     * Getter accessor for attribute 'users'.
     *
     * @return current value of 'users'
     */
    public Map<String, String> getUsers() {
        return users;
    }

    /**
     * Setter accessor for attribute 'users'.
     * 
     * @param users
     *            new value for 'users '
     */
    public void setUsers(Map<String, String> users) {
        this.users = users;
    }

    /**
     * Getter accessor for attribute 'apiKeys'.
     *
     * @return current value of 'apiKeys'
     */
    public Set<String> getApiKeys() {
        return apiKeys;
    }

    /**
     * Setter accessor for attribute 'apiKeys'.
     * 
     * @param apiKeys
     *            new value for 'apiKeys '
     */
    public void setApiKeys(Set<String> apiKeys) {
        this.apiKeys = apiKeys;
    }

    /**
     * Getter accessor for attribute 'permissions'.
     *
     * @return current value of 'permissions'
     */
    public Map<String, Set<String>> getPermissions() {
        return permissions;
    }

    /**
     * Setter accessor for attribute 'permissions'.
     * 
     * @param permissions
     *            new value for 'permissions '
     */
    public void setPermissions(Map<String, Set<String>> permissions) {
        this.permissions = permissions;
    }

}
