package org.ff4j.web;

import static org.ff4j.web.FF4jWebConstants.ROLE_READ;
import static org.ff4j.web.FF4jWebConstants.ROLE_WRITE;

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


import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.FF4j;

/**
 * Bean to configure security for the WebAPI. This custom bean is defined to limit dependencies to Spring security for instance.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class ApiConfig implements FF4jProvider {
    
    /** Configuration of ff4j. */
    private FF4j ff4j;

    /** Enable authentication for jersey. */
    private boolean authenticate = false;
    
    /** Enable authorization for jersey. */
    private boolean autorize = false;
    
    /** Enable logging filter for jersey. */
    private boolean log = false;
    
    /** Enable Swagger Documentation. */
    private boolean documentation = true;
    
    /** Number. */
    private String version = getClass().getPackage().getImplementationVersion();
    
    /** User of the API (login/password). */
    private Map<String, String> users = new HashMap<String, String>();

    /** will hold granted api Keys. */
    private Set<String> apiKeys = new HashSet<String>();

    /** will hold all permissions for both user and apiKey. */
    private Map<String, Set<String>> permissions = new HashMap<String, Set<String>>();
    
    private String host = "localhost";
    
    private int port = 8282;
    
    private String webContext = "ff4j-demo";
   
    /**
     * Default constructor.
     */
    public ApiConfig() {
        version = getClass().getPackage().getImplementationVersion();
    }
    
    /**
     * Initialized with a ff4j.
     */
    public ApiConfig(FF4j ff) {
        this();
        this.ff4j = ff;
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
    public ApiConfig createUser(String userName, String password, boolean read, boolean write, Set <String > usrPerm) {
        users.put(userName, password);
        Set<String> tmpPerm = new HashSet<String>();
        if (read) {
            tmpPerm.add(ROLE_READ);
        }
        if (write) {
            tmpPerm.add(ROLE_WRITE);
        }
        if (usrPerm != null) {
            tmpPerm.addAll(usrPerm);
        }
        permissions.put(userName, tmpPerm);
        return this;
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
    public ApiConfig createApiKey(String apiKey, boolean read, boolean write, Set <String > usrPerm) {
        apiKeys.add(apiKey);
        Set<String> tmpPerm = new HashSet<String>();
        if (read) {
            tmpPerm.add(ROLE_READ);
        }
        if (write) {
            tmpPerm.add(ROLE_WRITE);
        }
        if (usrPerm != null) {
            tmpPerm.addAll(usrPerm);
        }
        permissions.put(apiKey, tmpPerm);
        return this;
    }

    /** {@inheritDoc} */
    @Override
    public FF4j getFF4j() {
        return ff4j;
    }

    /**
     * Setter accessor for attribute 'fF4j'.
     * 
     * @param fF4j
     *            new value for 'fF4j '
     */
    public void setFF4j(FF4j fF4j) {
        this.ff4j = fF4j;
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

    /**
     * Getter accessor for attribute 'contextPath'.
     *
     * @return
     *       current value of 'contextPath'
     */
    public String getContextPath() {
        return  "http://" + getHost() + ":" + getPort() + "/" + getWebContext() + "/api";
    }

    /**
     * Getter accessor for attribute 'host'.
     *
     * @return
     *       current value of 'host'
     */
    public String getHost() {
        return host;
    }

    /**
     * Setter accessor for attribute 'host'.
     * @param host
     * 		new value for 'host '
     */
    public void setHost(String host) {
        this.host = host;
    }

    /**
     * Getter accessor for attribute 'port'.
     *
     * @return
     *       current value of 'port'
     */
    public int getPort() {
        return port;
    }

    /**
     * Setter accessor for attribute 'port'.
     * @param port
     * 		new value for 'port '
     */
    public void setPort(int port) {
        this.port = port;
    }

    /**
     * Getter accessor for attribute 'webContext'.
     *
     * @return
     *       current value of 'webContext'
     */
    public String getWebContext() {
        return webContext;
    }

    /**
     * Setter accessor for attribute 'webContext'.
     * @param webContext
     * 		new value for 'webContext '
     */
    public void setWebContext(String webContext) {
        this.webContext = webContext;
    }

    /**
     * Getter accessor for attribute 'authenticate'.
     *
     * @return
     *       current value of 'authenticate'
     */
    public boolean isAuthenticate() {
        return authenticate;
    }

    /**
     * Setter accessor for attribute 'authenticate'.
     * @param authenticate
     * 		new value for 'authenticate '
     */
    public void setAuthenticate(boolean authenticate) {
        this.authenticate = authenticate;
    }

    /**
     * Getter accessor for attribute 'autorize'.
     *
     * @return
     *       current value of 'autorize'
     */
    public boolean isAutorize() {
        return autorize;
    }

    /**
     * Setter accessor for attribute 'autorize'.
     * @param autorize
     * 		new value for 'autorize '
     */
    public void setAutorize(boolean autorize) {
        this.autorize = autorize;
    }

    /**
     * Getter accessor for attribute 'log'.
     *
     * @return
     *       current value of 'log'
     */
    public boolean isLog() {
        return log;
    }

    /**
     * Setter accessor for attribute 'log'.
     * @param log
     * 		new value for 'log '
     */
    public void setLog(boolean log) {
        this.log = log;
    }

    /**
     * Getter accessor for attribute 'documentation'.
     *
     * @return
     *       current value of 'documentation'
     */
    public boolean isDocumentation() {
        return documentation;
    }

    /**
     * Setter accessor for attribute 'documentation'.
     * @param documentation
     * 		new value for 'documentation '
     */
    public void setDocumentation(boolean documentation) {
        this.documentation = documentation;
    }

    /**
     * Getter accessor for attribute 'version'.
     *
     * @return
     *       current value of 'version'
     */
    public String getVersion() {
        return version;
    }

    /**
     * Setter accessor for attribute 'version'.
     * @param version
     * 		new value for 'version '
     */
    public void setVersion(String version) {
        this.version = version;
    }

}
