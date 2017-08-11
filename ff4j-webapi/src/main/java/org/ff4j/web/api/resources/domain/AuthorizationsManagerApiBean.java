package org.ff4j.web.api.resources.domain;

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

import java.util.ArrayList;
import java.util.List;

import org.ff4j.security.AuthorizationsManager;

import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;

@ApiModel( value = "authorization", description = "resource representation of a security" )
public class AuthorizationsManagerApiBean {

    /**
     * Implementation class of the authentication manager.
     */
    @JsonProperty("type")
    private String type = "";
    
    /**
     * All Permissions available
     */
    @JsonProperty("permissions")
    private List < String > permissions = new ArrayList<String>();
    
    /**
     * Default Constructor.
     */
    public AuthorizationsManagerApiBean() {
    }
            
    /**
     * Copy constructor.
     *
     * @param authMger
     *      target authentifaction manager
     */
    public AuthorizationsManagerApiBean(AuthorizationsManager authMger) {
        type = authMger.getClass().getName();
        permissions = new ArrayList<String>(authMger.listAllPermissions());
    }

    /**
     * Getter accessor for attribute 'type'.
     *
     * @return
     *       current value of 'type'
     */
    public String getType() {
        return type;
    }

    /**
     * Setter accessor for attribute 'type'.
     * @param type
     * 		new value for 'type '
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Getter accessor for attribute 'permissions'.
     *
     * @return
     *       current value of 'permissions'
     */
    public List<String> getPermissions() {
        return permissions;
    }

    /**
     * Setter accessor for attribute 'permissions'.
     * @param permissions
     * 		new value for 'permissions '
     */
    public void setPermissions(List<String> permissions) {
        this.permissions = permissions;
    }
}
