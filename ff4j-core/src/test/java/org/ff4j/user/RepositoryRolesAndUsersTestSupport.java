package org.ff4j.user;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2018 FF4J
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

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Map;
import java.util.stream.Stream;

import org.ff4j.FF4j;
import org.ff4j.exception.AssertionViolationException;
import org.ff4j.parser.ConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.test.AssertFF4j;
import org.ff4j.test.FF4jTestDataSet;
import org.ff4j.user.exception.RoleNotFoundException;
import org.ff4j.user.exception.UserNotFoundException;
import org.ff4j.user.repository.RolesAndUsersRepository;
import org.ff4j.utils.Util;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Users and roles.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class RepositoryRolesAndUsersTestSupport implements FF4jTestDataSet {
    
    /** Initialize */
    protected FF4j ff4j = null;

    /** Tested Store. */
    protected RolesAndUsersRepository testedStore;

    /** Test Values */
    protected AssertFF4j assertFF4j;
    
    /** DataSet. **/
    protected FF4jConfigFile testDataSet;
    
    /** {@inheritDoc} */
    @BeforeEach
    public void setUp() throws Exception {
        ConfigurationFileParser.clearCache();
        ff4j        = new FF4j().withRepositoryUsersRoles(initStore());
        assertFF4j  = new AssertFF4j(ff4j);
        testedStore = ff4j.getRepositoryUsersRoles();
        testDataSet = expectConfig();
    }
    
    /**
     * Any store test will declare its store through this callback.
     * 
     * @return working feature store
     * @throws Exception
     *          Hi guys, just let you know I did the update in the presentation : changing instructors names to put the 2 of you    error during building feature store
     */
    protected abstract RolesAndUsersRepository initStore();
    
    @Test
    @DisplayName("When test roles existence with null param, expecting AssertionViolationException")
    public void existWithNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.exists(null); });
        assertThrows(AssertionViolationException.class, () -> { testedStore.exists(""); });
    }
    
    @Test
    @DisplayName("When creating properties with null param, expecting AssertionViolationException")
    public void createWithNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.save((FF4jUser)null); });
    }
    
    @Test
    @DisplayName("When creating new user, it becomes available")
    public void createShouldMakeNewUseryAvailable() throws Exception {
        // Given
        assertFF4j.assertThatUserStoreHasSize(testDataSet.getUsers().size());
        assertFF4j.assertThatUserDoesNotExist(USER_FOR_TEST);
        // When
        testedStore.save(new FF4jUser(USER_FOR_TEST));
        // Then
        assertFF4j.assertThatUserStoreHasSize(testDataSet.getUsers().size() + 1);
        assertFF4j.assertThatUserExist(USER_FOR_TEST);
    }
    
    @Test
    @DisplayName("When configuration file is null, expecting violation exception")
    public void readWithNullShouldThrowViolationException() {
        assertThrows(AssertionViolationException.class, () -> { testedStore.read(null); });
    }
    
    @Test
    @DisplayName("When configuration file is empty, expecting violation exception")
    public void readWithEmptyShouldThrowViolationException() {
        assertThrows(AssertionViolationException.class, () -> { testedStore.read(""); });
    }
    
    @Test
    @DisplayName("When parsing configuration file, should have expected test DataSet")
    public void readAllConfFileShouldMatchExpectedDataSet() {
        // Given
        assertFF4j.assertThatUserStoreHasSize(testDataSet.getUsers().size());
        // When
        Map < String, FF4jUser > users = Util.toMap(testedStore.findAll());
        // Then
        Assertions.assertEquals(testDataSet.getUsers().size(), users.size());
        Assertions.assertTrue(users.containsKey(USER_JOHN));
        Assertions.assertTrue(users.get(USER_JOHN).getRoles().contains(ROLE_ADMIN));
        Assertions.assertTrue(users.containsKey(USER_SARAH));
    }
    
    // --- update ---
    
    @Test
    @DisplayName("When updating user with null param, expecting AssertionViolationException")
    public void updateUserWithNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { 
            testedStore.save((FF4jUser) null); 
        });
    }
    
    @Test
    @DisplayName("When updating user, all attributes should be updated")
    public void testUpdateUserProperties() {
        // Givens
        String newDescription = "new-description";
        assertFF4j.assertThatUserExist(USER_JOHN);
        Assertions.assertTrue(testedStore.read(USER_JOHN).getDescription().isPresent());
        Assertions.assertNotEquals(newDescription, testedStore.read(USER_JOHN).getDescription().get());
        Assertions.assertFalse(testedStore.read(USER_JOHN).getRoles().contains(ROLE_NEW));
        // When
        FF4jUser currentUser = testedStore.read(USER_JOHN);
        currentUser.setDescription(newDescription);
        currentUser.addRole(ROLE_NEW);
        testedStore.save(currentUser);
        // Then
        FF4jUser updatedUser = testedStore.read(USER_JOHN);
        Assertions.assertEquals(newDescription, testedStore.read(USER_JOHN).getDescription().get());
        Assertions.assertTrue(updatedUser.getRoles().contains(ROLE_NEW));
    }
    
    // --- delete ---
    
    @Test
    @DisplayName("When deleting new user, it is not available anymore")
    public void deleteShouldRemoveExistence() throws Exception {
        assertFF4j.assertThatUserStoreHasSize(testDataSet.getUsers().size());
        assertFF4j.assertThatUserExist(USER_JOHN);
        testedStore.delete(USER_JOHN);
        assertFF4j.assertThatUserStoreHasSize(testDataSet.getUsers().size() - 1 );
        assertFF4j.assertThatUserDoesNotExist(USER_JOHN);
    }
    
    @Test
    @DisplayName("When deleting new role, it is not available anymore")
    public void deleteRoleShouldRemoveExistence() throws Exception {
        assertFF4j.assertThatRoleStoreHasSize(testDataSet.getRoles().size());
        assertFF4j.assertThatRoleExist(ROLE_ADMIN);
        testedStore.deleteRole(ROLE_ADMIN);
        assertFF4j.assertThatRoleStoreHasSize(testDataSet.getRoles().size() - 1 );
        assertFF4j.assertThatRoleDoesNotExist(ROLE_ADMIN);
    }

    @Test
    @DisplayName("When deleting user with null param, expecting AssertionViolationException")
    public void deleteUserWithNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.delete((String) null); });
    }
    
    @Test
    @DisplayName("When deleting role with null param, expecting AssertionViolationException")
    public void deleteRoleWithNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.deleteRole((String) null); });
    }

    @Test
    @DisplayName("When deleting user with null param, expecting UserNotFoundException")
    public void deleteUnknownuserShouldThrowUserNotFound() throws Exception {
        assertFF4j.assertThatUserDoesNotExist(USER_FOR_TEST);
        assertThrows(UserNotFoundException.class, () -> { 
            testedStore.delete(USER_FOR_TEST); 
        });
    }
    
    @Test
    @DisplayName("When deleting role with null param, expecting UserNotFoundException")
    public void deleteUnknownRoleShouldThrowUserNotFound() throws Exception {
        assertFF4j.assertThatRoleDoesNotExist(ROLE_NEW);
        assertThrows(RoleNotFoundException.class, () -> { 
            testedStore.deleteRole(ROLE_NEW); 
        });
    }
    
    @Test
    @DisplayName("When invoking clear all users are deleted")
    public void clearUsersShouldEmptyRepository() {
        assertFF4j.assertThatUserExist(USER_JOHN);
        assertFF4j.assertThatUserExist(USER_SARAH);
        testedStore.deleteAll();
        // Then
        Assertions.assertEquals(0, testedStore.findAll().count());
    }
    
    @Test
    @DisplayName("When invoking clear all users are deleted")
    public void clearRolesShouldEmptyRepository() {
        assertFF4j.assertThatRoleExist(ROLE_ADMIN);
        assertFF4j.assertThatRoleExist(ROLE_USER);
        testedStore.deleteAllRoles();
        // Then
        Assertions.assertEquals(0, testedStore.countRoles());
    }
    
    
    @Test
    @DisplayName("When listing all user names, the are all retrieve")
    public void listingUserNamesShouldRetrieveAllNames() {
        Stream< String> userNames = testedStore.listUsersNames();
        Stream< String> roleNames = testedStore.listRoleNames();
        Assertions.assertNotNull(userNames);
        Assertions.assertNotNull(roleNames);
        
        Assertions.assertEquals(testDataSet.getUsers().size(), userNames.count());
        Assertions.assertEquals(testDataSet.getUsers().size(),  testedStore.count());
        
        Assertions.assertEquals(testDataSet.getRoles().size(), roleNames.count());
        Assertions.assertEquals(testDataSet.getRoles().size(),  testedStore.countRoles());
    }
    
    
}
