package org.ff4j.web.store;

import org.ff4j.FF4j;

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

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.ff4j.web.jersey1.store.FeatureStoreHttp;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import com.sun.jersey.test.framework.JerseyTest;

import static org.ff4j.test.TestsFf4jConstants.*;

/**
 * Unitary test for {@link FeatureStoreHttp} on Grizzly server.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreHttpTest extends FeatureStoreTestSupport {

    /** Jersy Test */
    private static JerseyTest jt = null;
    
    /**
     * Start Server Grizzly before tests on remote FeatureStore.
     */
    @BeforeClass
    public static void initializingInMemory() throws Exception {
        // Reinit FF4J
        FeatureStoreHttpTestIT.ff4j = new FF4j(TEST_FEATURES_FILE);
        jt = new FeatureStoreHttpTestIT();
        jt.setUp();
    }
    
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        FeatureStoreHttp fst = new FeatureStoreHttp("http://localhost:9998/ff4j");
        return fst;
    }

    /**
     * TDD.
     */
    @Test
    @Override
    public void testGrantRoleToFeatureRoleDoesNotExist() throws Exception {
        // Given
        assertFf4j.assertThatFeatureExist(F1);
        assertFf4j.assertThatFeatureHasNotRole(F1, ROLE_NEW);
        // When
        testedStore.grantRoleOnFeature(F1, ROLE_NEW);
        // Then
        assertFf4j.assertThatFeatureHasRole(F1, ROLE_NEW);
    }

    /**
     * TDD.
     */
    @Override
    @Test
    public void testStoreHasBeenInitialized() {
        // Given
        assertFf4j.assertThatStoreHasSize(EXPECTED_FEATURES_NUMBERS);
        assertFf4j.assertThatFeatureFlipped(F1);
    }

    /**
     * TDD.
     */
    @Override
    @Test
    public void testUpdateFlipLessAutorisation() {
        // Given
        assertFf4j.assertThatFeatureExist(F1);
        assertFf4j.assertThatFeatureHasRole(F1, ROLE_USER);
        // When
        testedStore.update(new Feature(F1, false, null));
        // Then
        Assert.assertTrue(testedStore.read(F1).getPermissions().isEmpty());
    }
    
    
    /**
     * TDD.
     */
    @Test
    public void testInitClient() {
        FeatureStoreHttp httpStore = new FeatureStoreHttp();
        httpStore.setUrl(null);
        Assert.assertNull(httpStore.getUrl());
        httpStore = new FeatureStoreHttp("a", "b", "c");
        httpStore = new FeatureStoreHttp(null, "Key");
    }

    /**
     * Start Server Grizzly before tests on FeatureStore
     * 
     * @throws Exception
     */
    @AfterClass
    public static void shutDownInMemeoryServer() throws Exception {
        jt.tearDown();
    }

}
