package org.ff4j.store;

import java.util.Date;

import org.ff4j.ehcache.FF4jEhCacheWrapper;
import org.ff4j.property.PropertyDate;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLogLevel.LogLevel;

/*
 * #%L
 * ff4j-store-ehcache
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Assert;
import org.junit.Test;

import net.sf.ehcache.config.Configuration;

/**
 * Work with properties
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */

public class PropertyStoreEhCacheTest extends PropertyStoreTestSupport {

    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreEhCache ehcachePStore = new PropertyStoreEhCache();
        ehcachePStore.importPropertiesFromXmlFile("ff4j-properties.xml");
        return ehcachePStore;
    }
    
    @Test
    public void initWithConfig() {
        Configuration managerConfiguration = new Configuration();
        managerConfiguration.name("config");
        PropertyStoreEhCache storeEHcache = new PropertyStoreEhCache(managerConfiguration);
        Assert.assertNotNull(storeEHcache);
    }
    
    @Test
    public void initWithXmlFile() {
        PropertyStoreEhCache storeEHcache = new PropertyStoreEhCache("ehcache.xml");
        Assert.assertNotNull(storeEHcache);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testInvalidXmlConfigFile() {
       new PropertyStoreEhCache("does-notexist.xml");
    }
    
    @Test
    public void testCacheWrapper() {
        FF4jEhCacheWrapper wrapper = new FF4jEhCacheWrapper("ehcache.xml");
        Assert.assertNotNull(wrapper.getCacheFeatures());
              
    }
    
    @Test
    public void testCacheWrapperObject() {
        Configuration managerConfiguration = new Configuration();
        managerConfiguration.name("config");
        FF4jEhCacheWrapper wrapper = new FF4jEhCacheWrapper(managerConfiguration);
        Assert.assertNotNull(wrapper.getCacheFeatures());
              
    }
    
    /** TDD. */
    @Test
    @Override
    public void addPropertyOKDate() {
        // Given
        //Assert.assertFalse(testedStore.exist("log"));
        // When
        testedStore.createProperty(new PropertyDate("ddateee", new Date()));
        // Then
        Assert.assertTrue(testedStore.existProperty("ddateee"));
    }
    
    @Test
    @Override
    public void readOKFixed() {
    }
    
    /** TDD. */
    @Test
    @Override
    public void updateOK() {
        // Given
        testedStore.createProperty(new PropertyLogLevel("updateOKK", LogLevel.ERROR));
        // When
        testedStore.updateProperty("updateOKK", "INFO");
        // Then
        Assert.assertEquals(LogLevel.INFO, testedStore.readProperty("updateOKK").getValue());
    }

    /** TDD. */
    @Test
    @Override
    public void addPropertyOKsimple() {
        System.out.println("Property simple");
    }
    

}
