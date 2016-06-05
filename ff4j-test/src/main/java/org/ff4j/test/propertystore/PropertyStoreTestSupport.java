package org.ff4j.test.propertystore;

import java.util.Date;
import java.util.Map;
import java.util.Set;

import org.ff4j.FF4j;

/*
 * #%L
 * ff4j-test
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import org.ff4j.core.FeatureStore;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.property.PropertyDate;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * SuperClass to test stores within core project
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class PropertyStoreTestSupport {

    public static final String ADD_PROPERTY_O_KSIMPLE = "addPropertyOKsimple";
    public static final String READ_OK_FIXED = "readOKFixed";
    public static final String INVALID = "invalid";
    public static final String UPDATE_OK = "updateOK";
    public static final String DELETE_OK = "deleteOK";
    
    /** Initialize */
    protected FF4j ff4j = null;
    
    /** Tested Store. */
    protected PropertyStore testedStore;

    /** Default InMemoryStore for test purposes. */
    protected FeatureStore defaultStore = new InMemoryFeatureStore();
    
    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        ff4j = new FF4j();
        ff4j.setPropertiesStore(initPropertyStore());
        testedStore = ff4j.getConcretePropertyStore();
    }

    /**
     * Any store test will declare its store through this callback.
     * 
     * @return working feature store
     * @throws Exception
     *             error during building feature store
     */
    protected abstract PropertyStore initPropertyStore();
    
    // --------------- exist -----------
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void existKONull() {
        // given
        testedStore.existProperty(null);
        // then expect to fail
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void existKOEmpty() {
        // Given
        testedStore.existProperty("");
        // Then
        // then expect to fail
    }
    
    /** TDD. */
    @Test
    public void existfalse() {
        // When-Then
        Assert.assertFalse(testedStore.existProperty("propertyThatNotExist"));
    }
    
    // --------------- create -----------    
    
    /** TDD. */
    @Test
    public void addPropertyOKsimple() {
        // Given
        Assert.assertFalse(testedStore.existProperty(ADD_PROPERTY_O_KSIMPLE));
        // When
        testedStore.createProperty(new PropertyString(ADD_PROPERTY_O_KSIMPLE, "ff4j"));
        // Then
        Assert.assertTrue(testedStore.existProperty(ADD_PROPERTY_O_KSIMPLE));
    }
    
    /** TDD. */
    @Test
    public void addPropertyOKLogLevel() {
        // Given
        //Assert.assertFalse(testedStore.exist("log"));
        // When
        testedStore.createProperty(new PropertyLogLevel("logi", LogLevel.DEBUG));
        // Then
        Assert.assertTrue(testedStore.existProperty("logi"));
    }
    
    /** TDD. */
    @Test
    public void addPropertyOKDate() {
        // Given
        //Assert.assertFalse(testedStore.exist("log"));
        // When
        testedStore.createProperty(new PropertyDate("ddate", new Date()));
        // Then
        Assert.assertTrue(testedStore.existProperty("ddate"));
    }
    
    /** TDD. */
    @Test(expected = PropertyAlreadyExistException.class)
    public void addPropertyKOAlreadyExist() {
        // Given
        testedStore.createProperty(new PropertyLogLevel("log", LogLevel.DEBUG));
        Assert.assertTrue(testedStore.existProperty("log"));
        // When
        testedStore.createProperty(new PropertyLogLevel("log", LogLevel.DEBUG));
        // Then expect to fail
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void addPropertyKONull() {
        // Given
        testedStore.createProperty(null);
        // Then expect to fail
    }
    
    // ------------------ read --------------------
    
    @Test
    public void readOK() {
        // Given
        testedStore.createProperty(new PropertyString("toto", "ff4j"));
        // When
        Property<?> ap = testedStore.readProperty("toto");
        // Then
        Assert.assertNotNull(ap);
        Assert.assertNotNull(ap.getName());
        Assert.assertEquals("toto", ap.getName());
        Assert.assertEquals("ff4j", ap.getValue());
        Assert.assertEquals("ff4j", ap.asString());
        Assert.assertNull(ap.getFixedValues());
        
    }
    
    @Test
    public void readOKFixed() {
        // Given
        testedStore.createProperty(new PropertyLogLevel(READ_OK_FIXED, LogLevel.ERROR));
        // When
        Property<?> log = testedStore.readProperty(READ_OK_FIXED);
        // Then
        Assert.assertNotNull(log);
        Assert.assertNotNull(log.getName());
        Assert.assertEquals(READ_OK_FIXED, log.getName());
        Assert.assertEquals(LogLevel.ERROR, log.getValue());
        Assert.assertEquals("ERROR", log.asString());
        Assert.assertNotNull(log.getFixedValues());
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void readKOnull() {
        // Given
        testedStore.readProperty(null);
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void readKOempty() {
        // Given
        testedStore.readProperty("");
    }
    
    /** TDD. */
    @Test(expected = PropertyNotFoundException.class)
    public void readKOnotExist() {
        // Given
        Assert.assertFalse(testedStore.existProperty(INVALID));
        // When
        testedStore.readProperty(INVALID);
    }
    
    // ------------------ update --------------------
    
    /** TDD. */
    @Test(expected = PropertyNotFoundException.class)
    public void updateKOdoesnotExist() {
        // Given
        Assert.assertFalse(testedStore.existProperty(INVALID));
        // When
        testedStore.updateProperty(INVALID, "aa");
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void updateKOnull() {
        // When
        testedStore.updateProperty(null, "aa");
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void updateKONullBis() {
        // When
        testedStore.updateProperty(null);
    }
    
    
    /** TDD. */
    @Test(expected = PropertyNotFoundException.class)
    public void updateKOPropertyNotFound() {
        // When
        PropertyString ps = new PropertyString("does-not-exist");
        testedStore.updateProperty(ps);
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void updateKOempty() {
        // When
        testedStore.updateProperty("", "aa");
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void updateKOInvalidValue() {
        // Given
        testedStore.createProperty(new PropertyLogLevel("updateKOInvalidValue", LogLevel.ERROR));
        // When
        testedStore.updateProperty("updateKOInvalidValue", "KO");
    }
    
    /** TDD. */
    @Test
    public void updateOK() {
        // Given
        testedStore.createProperty(new PropertyLogLevel(UPDATE_OK, LogLevel.ERROR));
        // When
        testedStore.updateProperty(UPDATE_OK, "INFO");
        // Then
        Assert.assertEquals(LogLevel.INFO, testedStore.readProperty(UPDATE_OK).getValue());
    }
    
    /** TDD. */
    @Test
    public void updateOKProperties() {
        // Given
        testedStore.createProperty(new PropertyLogLevel("logX", LogLevel.ERROR));
        // When
        PropertyLogLevel pll = new PropertyLogLevel("logX", LogLevel.INFO);
        testedStore.updateProperty(pll);
        // Then
        Assert.assertEquals(LogLevel.INFO, testedStore.readProperty("logX").getValue());
    }
    
    // ------------------ delete -------------------- 

    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void deleteKOnull() {
        // When
        testedStore.deleteProperty(null);
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void deleteKOempty() {
        // When
        testedStore.deleteProperty("");
    }
    
    /** TDD. */
    @Test(expected = PropertyNotFoundException.class)
    public void deleteKOdoesnotexist() {
        // Given
        Assert.assertFalse(testedStore.existProperty(INVALID));
        // When
        testedStore.deleteProperty(INVALID);
    }
    
    /** TDD. */
    @Test
    public void deleteOK() {
        // Given
        testedStore.createProperty(new PropertyString(DELETE_OK, "ff4j"));
        Assert.assertTrue(testedStore.existProperty(DELETE_OK));
        // When
        testedStore.deleteProperty(DELETE_OK);
        // Then
        Assert.assertFalse(testedStore.existProperty(DELETE_OK));
    }
    
    @Test
    public void existfilled() {
        // When-Then
        Assert.assertTrue(testedStore.existProperty("a"));
        Assert.assertFalse(testedStore.existProperty("koala"));
    }
    
    @Test
    public void valueFixed() {
        // When-Then
        Assert.assertTrue(testedStore.existProperty("a"));
        Assert.assertEquals("AMER", testedStore.readProperty("a").getValue());
    }
    
    /** TDD. */
    @Test
    public void listPropertyNames() {
        // Given, When
        Set< String > proNames = testedStore.listPropertyNames();
        // Then
        Assert.assertTrue(proNames.contains("a"));
    }
    
    /** TDD. */
    @Test
    public void readAllProperties() {
        // Given
        Assert.assertNotNull(testedStore);
        // When
        Map <String, Property<?>> mapsOf = testedStore.readAllProperties();
        // When
        Assert.assertTrue(mapsOf.containsKey("a"));
        Assert.assertTrue(mapsOf.containsKey("b"));
    }
    
    /** TDD. */
    @Test
    public void clear() {
        // Given
        Assert.assertNotNull(testedStore);
        Map <String, Property<?>> before = testedStore.readAllProperties();
        Assert.assertFalse(before.isEmpty());
        // When
        testedStore.clear();
        // Then
        Assert.assertTrue(testedStore.readAllProperties().isEmpty());
        
        /// Reinit
        for (Map.Entry<String,Property<?>> pName : before.entrySet()) {
            testedStore.createProperty(pName.getValue());
        }
    }
    

    
}
