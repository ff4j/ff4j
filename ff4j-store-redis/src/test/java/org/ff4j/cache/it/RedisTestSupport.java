package org.ff4j.cache.it;

/*-
 * #%L
 * ff4j-store-redis
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import static org.ff4j.test.TestsFf4jConstants.TEST_FEATURES_FILE;

import org.ff4j.FF4j;
import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.conf.XmlParser;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.AssertFf4j;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Provide support to run Redis in a testcontainer
 *
 * @author <a href="https://github.com/mrgrew">Greg Wiley</a>
 */
public abstract class RedisTestSupport {

    /** Initial sizes */
    public static final int INITIAL_FEATURES_SIZE = 5;
    public static final int INITIAL_PROPERTIES_SIZE = 12;

    // @Rule
    // public GenericContainer<?> redis = new GenericContainer<>("redis").withExposedPorts(6379);

    protected abstract FF4JCacheManager makeCache();

    /** Test Values */
    protected AssertFf4j assertFf4j;

    /** Initialize */
    protected FF4j ff4j;

    /** Cache Manager. */
    protected FF4JCacheManager cache;

    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        cache = makeCache();
        ff4j = new FF4j(new XmlParser(), TEST_FEATURES_FILE);
        ff4j.cache(cache);
        assertFf4j = new AssertFf4j(ff4j);
    }

    @After
    public void tearDown() throws Exception {
        cache.clearFeatures();
        cache.clearProperties();
    }

    @Test
    public void testStoresAndCacheHaveBeenInitialized() {
        assertFf4j.assertThatStoreHasSize(INITIAL_FEATURES_SIZE);
        Assert.assertTrue(cache.listCachedPropertyNames().isEmpty());
    }

    @Test
    public void testOnePropertyIsCachedThenCleared() {
        // Given
        Assert.assertTrue(cache.listCachedPropertyNames().isEmpty());
        // When
        ff4j.getProperty("a");
        // Then
        Assert.assertEquals(1, cache.listCachedPropertyNames().size());
        // When
        cache.clearProperties();
        // Then
        Assert.assertTrue(cache.listCachedPropertyNames().isEmpty());
    }

    @Test
    public void testAllPropertiesAreCachedThenCleared() {
        // Given
        Assert.assertTrue(cache.listCachedPropertyNames().isEmpty());
        // When
        PropertyStore propertyStore = ff4j.getPropertiesStore();
        propertyStore.listPropertyNames().forEach(propertyStore::readProperty);
        // Then
        Assert.assertEquals(propertyStore.readAllProperties().size(), cache.listCachedPropertyNames().size());
        // When
        cache.clearProperties();
        // Then
        Assert.assertTrue(cache.listCachedPropertyNames().isEmpty());
    }

    @Test
    public void testOneProperty() {
        // Given
        Assert.assertTrue(cache.listCachedPropertyNames().isEmpty());
        // When
        ff4j.getProperty("a");
        // Then
        Assert.assertEquals(1, cache.listCachedPropertyNames().size());
    }

}
