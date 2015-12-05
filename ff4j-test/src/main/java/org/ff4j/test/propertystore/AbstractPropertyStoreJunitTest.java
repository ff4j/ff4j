package org.ff4j.test.propertystore;

import java.util.Date;

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
import org.ff4j.property.AbstractProperty;
import org.ff4j.property.Property;
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
public abstract class AbstractPropertyStoreJunitTest {

    /** Tested Store. */
    protected PropertyStore testedStore;

    /** Default InMemoryStore for test purposes. */
    protected FeatureStore defaultStore = new InMemoryFeatureStore();
    
    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        testedStore = initPropertyStore();
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
    public void existKO_Null() {
        // given
        testedStore.existProperty(null);
        // then expect to fail
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void existKO_Empty() {
        // Given
        testedStore.existProperty("");
        // Then
        // then expect to fail
    }
    
    /** TDD. */
    @Test
    public void exist_false() {
        // When-Then
        Assert.assertFalse(testedStore.existProperty("propertyThatNotExist"));
    }
    
    // --------------- create -----------    
    
    /** TDD. */
    @Test
    public void addPropertyOK_simple() {
        // Given
        Assert.assertFalse(testedStore.existProperty("addPropertyOK_simple"));
        // When
        testedStore.createProperty(new Property("addPropertyOK_simple", "ff4j"));
        // Then
        Assert.assertTrue(testedStore.existProperty("addPropertyOK_simple"));
    }
    
    /** TDD. */
    @Test
    public void addPropertyOK_LogLevel() {
        // Given
        //Assert.assertFalse(testedStore.exist("log"));
        // When
        testedStore.createProperty(new PropertyLogLevel("log", LogLevel.DEBUG));
        // Then
        Assert.assertTrue(testedStore.existProperty("log"));
    }
    
    /** TDD. */
    @Test
    public void addPropertyOK_Date() {
        // Given
        //Assert.assertFalse(testedStore.exist("log"));
        // When
        testedStore.createProperty(new PropertyDate("ddate", new Date()));
        // Then
        Assert.assertTrue(testedStore.existProperty("ddate"));
    }
    
    /** TDD. */
    @Test(expected = PropertyAlreadyExistException.class)
    public void addPropertyKO_AlreadyExist() {
        // Given
        testedStore.createProperty(new PropertyLogLevel("log", LogLevel.DEBUG));
        Assert.assertTrue(testedStore.existProperty("log"));
        // When
        testedStore.createProperty(new PropertyLogLevel("log", LogLevel.DEBUG));
        // Then expect to fail
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void addPropertyKO_Null() {
        // Given
        testedStore.createProperty(null);
        // Then expect to fail
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void addPropertyKO_NullName() {
        // Given
        testedStore.createProperty(new Property(null, ""));
        // Then expect to fail
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void addPropertyKO_EmptyName() {
        // Given
        testedStore.createProperty(new Property("", ""));
        // Then expect to fail
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void addPropertyKO_NullValue() {
        // Given
        testedStore.createProperty(new Property("hi", null));
        // Then No error
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void addPropertyKO_InvalidValue() {
        // Given
        testedStore.createProperty(new PropertyLogLevel("log", "TRUC"));
        // Then No error
    }
    
    
    // ------------------ read --------------------
    
    @Test
    public void readOK() {
        // Given
        testedStore.createProperty(new Property("toto", "ff4j"));
        // When
        AbstractProperty<?> ap = testedStore.readProperty("toto");
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
        testedStore.createProperty(new PropertyLogLevel("readOKFixed", LogLevel.ERROR));
        // When
        AbstractProperty<?> log = testedStore.readProperty("readOKFixed");
        // Then
        Assert.assertNotNull(log);
        Assert.assertNotNull(log.getName());
        Assert.assertEquals("readOKFixed", log.getName());
        Assert.assertEquals(LogLevel.ERROR, log.getValue());
        Assert.assertEquals("ERROR", log.asString());
        Assert.assertNotNull(log.getFixedValues());
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void readKO_null() {
        // Given
        testedStore.readProperty(null);
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void readKO_empty() {
        // Given
        testedStore.readProperty("");
        // Expected error
        Assert.fail();
    }
    
    /** TDD. */
    @Test(expected = PropertyNotFoundException.class)
    public void readKO_notExist() {
        // Given
        Assert.assertFalse(testedStore.existProperty("invalid"));
        // When
        testedStore.readProperty("invalid");
        // Expected error
        Assert.fail();
    }
    
    // ------------------ update --------------------
    
    /** TDD. */
    @Test(expected = PropertyNotFoundException.class)
    public void updateKO_doesnotExist() {
        // Given
        Assert.assertFalse(testedStore.existProperty("invalid"));
        // When
        testedStore.updateProperty("invalid", "aa");
        // Expected error
        Assert.fail();
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void updateKO_null() {
        // When
        testedStore.updateProperty(null, "aa");
        // Expected error
        Assert.fail();
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void updateKO_empty() {
        // When
        testedStore.updateProperty("", "aa");
        // Expected error
        Assert.fail();
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void updateKO_InvalidValue() {
        // Given
        testedStore.createProperty(new PropertyLogLevel("updateKO_InvalidValue", LogLevel.ERROR));
        // When
        testedStore.updateProperty("updateKO_InvalidValue", "KO");
        // Expected error
        Assert.fail();
        
    }
    
    /** TDD. */
    @Test
    public void updateOK() {
        // Given
        testedStore.createProperty(new PropertyLogLevel("updateOK", LogLevel.ERROR));
        // When
        testedStore.updateProperty("updateOK", "INFO");
        // Then
        Assert.assertEquals(LogLevel.INFO, testedStore.readProperty("updateOK").getValue());
    }
    
    // ------------------ delete -------------------- 

    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void deleteKO_null() {
        // When
        testedStore.deleteProperty(null);
        // Expected Error
        Assert.fail();
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void deleteKO_empty() {
        // When
        testedStore.deleteProperty("");
        // Expected Error
        Assert.fail();
    }
    
    /** TDD. */
    @Test(expected = PropertyNotFoundException.class)
    public void deleteKO_doesnotexist() {
        // Given
        Assert.assertFalse(testedStore.existProperty("invalid"));
        // When
        testedStore.deleteProperty("invalid");
        // Expected Error
        Assert.fail();
    }
    
    /** TDD. */
    @Test
    public void deleteOK() {
        // Given
        testedStore.createProperty(new Property("deleteOK", "ff4j"));
        Assert.assertTrue(testedStore.existProperty("deleteOK"));
        // When
        testedStore.deleteProperty("deleteOK");
        // Then
        Assert.assertFalse(testedStore.existProperty("deleteOK"));
    }
    
    @Test
    public void exist_filled() {
        // When-Then
        Assert.assertTrue(testedStore.existProperty("a"));
        Assert.assertFalse(testedStore.existProperty("k"));
    }
    
    @Test
    public void valueFixed() {
        // When-Then
        Assert.assertTrue(testedStore.existProperty("a"));
        Assert.assertEquals("AMER", testedStore.readProperty("a").getValue());
    }

    
}
