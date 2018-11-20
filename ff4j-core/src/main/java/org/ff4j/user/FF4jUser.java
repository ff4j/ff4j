package org.ff4j.user;

import static org.ff4j.utils.JsonUtils.attributeAsJson;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import org.ff4j.FF4jEntity;
import org.ff4j.security.FF4jPermission;
import org.ff4j.utils.JsonUtils;

/**
 * Represent a user in FF4J.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public class FF4jUser extends FF4jEntity < FF4jUser > {
   
    /** serialVersionUID. */
    private static final long serialVersionUID = 7083552676589401961L;
    
    /** first Name. */
    private String firstName;
    
    /** Last Name. */
    private String lastName;
    
    /** Extra permissions if relevant. */
    private Set < FF4jPermission > permissions = new HashSet<>();
    
    /** User groups. */
    private Set < String > roles = new HashSet<>();
    
    /**
     * Create a user by its userName.
     *
     * @param uid
     *      user unique identifier
     */
    public FF4jUser(String uid) {
        super(uid);
    }
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }

    /**
     * Convert Feature to JSON.
     * 
     * @return target json
     */
    public String toJson() {
        StringBuilder json = new StringBuilder("{");
        json.append(super.baseJson());
        json.append(attributeAsJson("firstName", firstName));
        json.append(attributeAsJson("lastName", lastName));
        json.append(",\"roles\":" + JsonUtils.collectionAsJson(roles));
        json.append(",\"permissions\":" + JsonUtils.collectionAsJson(permissions));
        json.append("}");
        return json.toString();
    }
    
    /**
     * Getter accessor for attribute 'firstName'.
     *
     * @return
     *       current value of 'firstName'
     */
    public String getFirstName() {
        return firstName;
    }

    /**
     * Setter accessor for attribute 'firstName'.
     * @param firstName
     * 		new value for 'firstName '
     */
    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    /**
     * Getter accessor for attribute 'lastName'.
     *
     * @return
     *       current value of 'lastName'
     */
    public String getLastName() {
        return lastName;
    }
    
    /**
     * Fluent API to work with lastname.
     */
    public FF4jUser lastName(String lastname) {
        setLastName(lastname);
        return this;
    }
    
    /**
     * Fluent API to work with lastname.
     */
    public FF4jUser fisrtName(String firstName) {
        setFirstName(firstName);
        return this;
    }

    /**
     * Setter accessor for attribute 'lastName'.
     * @param lastName
     * 		new value for 'lastName '
     */
    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    /**
     * Fluent API to work with permission.
     *
     * @param permission
     *      new permission to grant
     * @return
     *      current object
     */
    public FF4jUser addPermission(FF4jPermission permission) {
        getPermissions().add(permission);
        return this;
    }
    
    /**
     * Fluent API to work with role.
     *
     * @param role
     *          new role to grant
     * @return
     *          current object
     */
    public FF4jUser addRole(String role) {
        getRoles().add(role);
        return this;
    }
    
    /**
     * Getter accessor for attribute 'permissions'.
     *
     * @return
     *       current value of 'permissions'
     */
    public Set<FF4jPermission> getPermissions() {
        return permissions;
    }

    /**
     * Setter accessor for attribute 'permissions'.
     * @param permissions
     * 		new value for 'permissions '
     */
    public void setPermissions(Set<FF4jPermission> permissions) {
        this.permissions = permissions;
    }

    /**
     * Getter accessor for attribute 'roles'.
     *
     * @return
     *       current value of 'roles'
     */
    public Set<String> getRoles() {
        return roles;
    }

    /**
     * Setter accessor for attribute 'roles'.
     * @param roles
     * 		new value for 'roles '
     */
    public void setRoles(Set<String> roles) {
        this.roles = roles;
    }
    
}
