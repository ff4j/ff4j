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

import org.ff4j.user.FF4jUser;

/**
 * Entities protected by permissions should implement this interface.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public interface RestrictedAccessObject {
    
    /** Get Access to control List of this object. */
    FF4jAcl getAccessControlList();
    
    /** Update modified date if relevant. */
    <T> T updateLastModifiedDate();
    
    default <T> T grantUser(String userName, FF4jPermission... perm) {
        assertHasLength(userName);
        assertNotNull(perm);
        FF4jAcl acl = getAccessControlList();
        assertNotNull(acl);
        acl.grantUser(userName, perm);
        return updateLastModifiedDate();
    }
    
    default <T> T grantUsers(FF4jPermission perm, String... users) {
        assertNotNull(perm);
        assertNotNull(users);
        FF4jAcl acl = getAccessControlList();
        assertNotNull(acl);
        acl.grantUsers(perm, users);
        return updateLastModifiedDate();
    }
    
    default <T> T grantRoles(FF4jPermission perm, String... roles) {
        assertNotNull(perm);
        assertNotNull(roles);
        FF4jAcl acl = getAccessControlList();
        assertNotNull(acl);
        acl.grantRoles(perm, roles);
        return updateLastModifiedDate();
    }
   
    default boolean isUserGranted(String userName, FF4jPermission perm) {
        assertHasLength(userName);
        assertNotNull(perm);
        FF4jAcl acl = getAccessControlList();
        assertNotNull(acl);
        return acl.isUserGranted(userName, perm);
    }
    
    default boolean isUserGranted(FF4jUser user, FF4jPermission perm) {
        assertNotNull(user);
        assertNotNull(perm);
        FF4jAcl acl = getAccessControlList();
        assertNotNull(acl);
        return acl.isUserGranted(user, perm);
    }
    
    default boolean isRoleGranted(String roleName, FF4jPermission perm) {
        assertHasLength(roleName);
        assertNotNull(perm);
        FF4jAcl acl = getAccessControlList();
        assertNotNull(acl);
        return acl.isRoleGranted(roleName, perm);
    }
    
}
