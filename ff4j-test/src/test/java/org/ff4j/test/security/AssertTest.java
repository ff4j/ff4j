package org.ff4j.test.security;

import java.lang.reflect.Constructor;

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


import org.ff4j.FF4j;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.AssertFf4j;
import org.ff4j.test.TestsFf4jConstants;
import org.ff4j.utils.Util;
import org.junit.Before;
import org.junit.Test;

public class AssertTest {
    
    private FF4j ff4j = null;
    
    private AssertFf4j assertFF4j = null;
    
    @Before
    public void initFF4J() {
        if (assertFF4j == null) {
            ff4j = new FF4j();
            ff4j.setFeatureStore(new InMemoryFeatureStore("test-ff4j-features.xml"));
            ff4j.setPropertiesStore(new InMemoryPropertyStore("test-ff4j-features.xml"));
            ff4j.setAuthorizationsManager(new DefaultAuthorisationManager(Util.set("PERM1", "PERM2"), Util.set("PERM1", "PERM2", "PERM3")));
            assertFF4j = new AssertFf4j(ff4j);
        }
    }
    
    @Test
    public void testUserNotAllowed() {
        assertFF4j.assertThatCurrentUserIsNotAllowedOnFeature("first");
    }
    
    @Test
    public void testExistProperty() {
        assertFF4j.assertThatPropertyExist("c");
        assertFF4j.assertThatPropertyDoesNotExist("toto");
        assertFF4j.assertThatPropertyStoreHasSize(12);
    }
    
    @Test
    public void testUserAllowed() {
        assertFF4j.assertThatCurrentUserIsAllowedOnFeature("AwesomeFeature");
    }
    
    @Test
    public void testAssertThatFeatureNotFlipped() {
        assertFF4j.assertThatFeatureNotFlipped("second");
    }
    
    @Test
    public void testAssertThatFeatureDoesNotHaveProperties() {
        assertFF4j.assertThatFeatureDoesNotHaveProperties("AwesomeFeature");
    }
    
    @Test
    public void testAssertThatFeatureNotInGroup() {
        assertFF4j.assertThatFeatureNotInGroup("AwesomeFeature", "GRP1");
        assertFF4j.assertThatFeatureNotInGroup("second", "GRP1");
    }
    
    @Test
    public void eventConstant() throws Exception {
        Constructor<TestsFf4jConstants> de = TestsFf4jConstants.class.getDeclaredConstructor();
        de.setAccessible(true);
        de.newInstance();
    }
}
