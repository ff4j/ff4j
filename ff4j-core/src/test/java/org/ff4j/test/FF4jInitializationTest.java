package org.ff4j.test;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
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

import static org.mockito.Mockito.mock;
import org.junit.Assert;

import org.ff4j.FF4j;
import org.ff4j.security.AuthorizationsManager;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Test;

/**
 * Test class for {@link FF4j} initializations.
 * 
 * This test will test FF4J class initializations through different constructor but also with static access.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class FF4jInitializationTest {

    /**
     * Factorisation of assertions for different ff4j initializations.
     */
    private void testingGeneratedFF4j(FF4j f) {
        Assert.assertNotNull(f.getFeatureStore());
        Assert.assertTrue(f.getFeatureStore() instanceof InMemoryFeatureStore);
        Assert.assertFalse(f.check("new"));
    }

    @Test
    public void testInitConstructor1() {
        FF4j f1 = new FF4j();
        f1.setAutocreate(true);
        testingGeneratedFF4j(f1);
    }

    @Test
    public void testInitConstructor2() {
        FF4j f2 = new FF4j();
        f2.setAutocreate(true);
        testingGeneratedFF4j(f2);
    }

    @Test
    public void testInitConstructor3() {
        FF4j f3 = new FF4j();
        f3.setAuthorizationsManager(mock(AuthorizationsManager.class));
        f3.autoCreate(true);

        testingGeneratedFF4j(f3);
        Assert.assertNotNull(f3.getAuthorizationsManager());
    }

}
