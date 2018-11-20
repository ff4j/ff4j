package org.ff4j.security;

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

import static org.ff4j.test.AssertUtils.assertHasLength;
import static org.ff4j.test.AssertUtils.assertNotNull;
import static org.ff4j.utils.JsonUtils.valueAsJson;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.ff4j.user.FF4jUser;

/**
 * Implementation of security mechanism.
 * 
 * @author Cedrick LUNVEN  (@clunven)
 */
public class FF4jAcl implements Serializable {

    /** serialVersionUID. */
    private static final long serialVersionUID = -7242564334708978726L;
    
    /** Permission : by Default everyOne can use the Feature. */
    private Map < FF4jPermission, FF4jGrantees > permissions = new HashMap<>();

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }
    
    /**
     * If the permissions map is empty;
     *
     * @return
     *      if the permission is empty.
     */
    public boolean isEmpty() {
        return permissions.isEmpty();
    }
    
    /**
     * Grant a permission to a target user.
     *
     * @param userName
     *      target userName
     * @param perms
     *      target permission
     */
    public void grantUser(String userName, FF4jPermission... perms) {
        assertHasLength(userName);
        assertNotNull(perms);
        Stream.of(perms).forEach(perm -> {
            if (!permissions.containsKey(perm)) {
                permissions.put(perm, new FF4jGrantees());
            }
            permissions.get(perm).grantUser(userName);
        });
    }
    
    public void revokeUser(String userName, FF4jPermission... perms) {
        assertHasLength(userName);
        assertNotNull(perms);
        Stream.of(perms).filter(permissions::containsKey)
                        .map(permissions::get)
                        .forEach(grantee -> grantee.revokeUser(userName));
    }
    
    public void grantRole(String roleName, FF4jPermission... perms) {
        assertHasLength(roleName);
        assertNotNull(perms);
        Stream.of(perms).forEach(perm -> {
            if (!permissions.containsKey(perm)) {
                permissions.put(perm, new FF4jGrantees());
            }
            permissions.get(perm).grantRole(roleName);
        });
    }
    
    void revokeRole(String roleName, FF4jPermission... perms) {
        assertHasLength(roleName);
        assertNotNull(perms);
        Stream.of(perms).filter(permissions::containsKey)
                        .map(permissions::get)
                        .forEach(grantee -> grantee.revokeRole(roleName));
    }
    
    /**
     * Grant a permission for several users.
     *
     * @param permission
     *      permission or right to set
     * @param users
     *      list of users
     */
    public void grantUsers(FF4jPermission permission, String... users)  {
        assertNotNull(permission);
        assertNotNull((Object[]) users);
        Stream.of(users).forEach(user -> grantUser(user, permission));
    }
    
    /**
     * Grant a set of groupNames to the permission.
     *
     * @param permission
     *          the right to work with
     * @param roles
     *          the groups to allow on this permission
     */
    public void grantRoles(FF4jPermission permission, String... roles)  {
        assertNotNull(permission);
        assertNotNull((Object[]) roles);
        Stream.of(roles).forEach(user -> grantRole(user, permission));
    } 
    
    /**
     * Check if a userName is allow to use it (no groups).
     *
     * @param userName
     * @param permission
     * @return
     */
    public boolean isRoleGranted(String roleName, FF4jPermission permission) {
        assertHasLength(roleName);
        assertNotNull(permission);
        return permissions.containsKey(permission) ? 
               permissions.get(permission).isRoleGranted(roleName) : false;
    }
    
    /**
     * Check if a userName is allow to use it (no groups).
     *
     * @param userName
     * @param permission
     * @return
     */
    public boolean isUserGranted(String userName, FF4jPermission permission) {
        assertHasLength(userName);
        assertNotNull(permission);
        return permissions.containsKey(permission) ? 
               permissions.get(permission).isUserGranted(userName) : false;
    }
    
    /**
     * Check if a userName.
     *
     * @param userName
     *      current userName
     * @param permission
     *      expected permission
     * @return
     *      if user is granted
     */
    public boolean isUserGranted(FF4jUser user, FF4jPermission permission) {
        assertNotNull(user);
        assertNotNull(permission);
        return permissions.containsKey(permission) ? 
               permissions.get(permission).isUserGranted(user) : false;
    }
    
    /**
     * Generate Json expression of rights.
     *
     * @return
     *      list of permissions
     */
    public String toJson() {
        StringBuilder json = new StringBuilder("{");
        boolean first = true;
        for (Map.Entry < FF4jPermission, FF4jGrantees > mapEntry : permissions.entrySet()) {
            json.append(first ? "" : ",");
            json.append("\"" + mapEntry.getKey() + "\":");
            json.append(valueAsJson(mapEntry.getValue()));
            first = false;
        }
        json.append("}");
        return json.toString();
    }

    /**
     * Getter accessor for attribute 'permissions'.
     *
     * @return
     *       current value of 'permissions'
     */
    public Map<FF4jPermission, FF4jGrantees> getPermissions() {
        return permissions;
    }

    /**
     * Setter accessor for attribute 'permissions'.
     * @param permissions
     * 		new value for 'permissions '
     */
    public void setPermissions(Map<FF4jPermission, FF4jGrantees> permissions) {
        this.permissions = permissions;
    }
    
}
