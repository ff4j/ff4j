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

import org.ff4j.FF4j;
import org.ff4j.exception.AssertionViolationException;
import org.ff4j.parser.ConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.property.PropertyString;
import org.ff4j.test.AssertFF4j;
import org.ff4j.test.FF4jTestDataSet;
import org.ff4j.user.repository.RolesAndUsersRepository;
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
    public void createShouldMakeNewPropertyAvailable() throws Exception {
        assertFF4j.assertThatUserStoreHasSize(testDataSet.getUsers().size());
        assertFF4j.assertThatUserDoesNotExist(USER_FOR_TEST);
        FF4jUser myNewUser = new FF4jUser(USER_FOR_TEST);
        myNewUser.addProperties(new PropertyString("p1", "v1"));
        testedStore.save(myNewUser);
        assertFF4j.assertThatUserStoreHasSize(testDataSet.getUsers().size() + 1);
        assertFF4j.assertThatUserExist(USER_FOR_TEST);
    }

}
