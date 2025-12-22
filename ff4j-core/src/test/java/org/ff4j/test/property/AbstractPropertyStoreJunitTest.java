package org.ff4j.test.property;

/*-
 * #%L
 * ff4j-core
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

import static org.junit.jupiter.api.Assertions.assertThrows;

/*-
 * #%L
 * ff4j-core
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

import java.util.Date;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.FeatureStore;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyDate;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * SuperClass to test stores within core project
 *
 * @author Cedrick Lunven (@clunven)
 */
public abstract class AbstractPropertyStoreJunitTest {

    /** Tested Store. */
    protected PropertyStore testedStore;

    /** Default InMemoryStore for test purposes. */
    protected FeatureStore defaultStore = new InMemoryFeatureStore();
    
    /** {@inheritDoc} */
    @BeforeEach
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
    
    @Test
    public void testEmptyStore() {
        Assertions.assertFalse(testedStore.isEmpty());
    }
    
    /** TDD. */
    @Test
    public void existKONull() {
        assertThrows(IllegalArgumentException.class, () ->
            // given
            testedStore.existProperty(null));
        // then expect to fail
    }
    
    /** TDD. */
    @Test
    public void existKOEmpty() {
        assertThrows(IllegalArgumentException.class, () ->
            // Given
            testedStore.existProperty(""));
        // Then
        // then expect to fail
    }
    
    /** TDD. */
    @Test
    public void existfalse() {
        // When-Then
        Assertions.assertFalse(testedStore.existProperty("toto"));
    }
    
    // --------------- create -----------    
    
    /** TDD. */
    @Test
    public void addPropertyOKsimple() {
        // Given
        Assertions.assertFalse(testedStore.existProperty("toto"));
        // When
        testedStore.createProperty(new PropertyString("toto", "ff4j"));
        // Then
        Assertions.assertTrue(testedStore.existProperty("toto"));
    }
    
    /** TDD. */
    @Test
    public void addPropertyOKLogLevel() {
        // Given
        //Assert.assertFalse(testedStore.exist("log"));
        // When
        testedStore.createProperty(new PropertyLogLevel("log", LogLevel.DEBUG));
        // Then
        Assertions.assertTrue(testedStore.existProperty("log"));
    }
    
    /** TDD. */
    @Test
    public void addPropertyOKDate() {
        // Given
        //Assert.assertFalse(testedStore.exist("log"));
        // When
        testedStore.createProperty(new PropertyDate("ddate", new Date()));
        // Then
        Assertions.assertTrue(testedStore.existProperty("ddate"));
    }
    
    /** TDD. */
    @Test
    public void addPropertyKOAlreadyExist() {
        assertThrows(PropertyAlreadyExistException.class, () -> {
            // Given
            testedStore.createProperty(new PropertyLogLevel("log", LogLevel.DEBUG));
            Assertions.assertTrue(testedStore.existProperty("log"));
            // When
            testedStore.createProperty(new PropertyLogLevel("log", LogLevel.DEBUG));
            // Then expect to fail
        });
        // Then expect to fail
    }
    
    /** TDD. */
    @Test
    public void addPropertyKONull() {
        assertThrows(IllegalArgumentException.class, () ->
            // Given
            testedStore.createProperty(null));
        // Then expect to fail
    }
    
    /** TDD. */
    @Test
    public void addPropertyKONullName() {
        assertThrows(IllegalArgumentException.class, () ->
            // Given
            testedStore.createProperty(new PropertyString(null, "")));
        // Then expect to fail
    }
    
    /** TDD. */
    @Test
    public void addPropertyKOEmptyName() {
        assertThrows(IllegalArgumentException.class, () ->
            // Given
            testedStore.createProperty(new PropertyString("", "")));
        // Then expect to fail
    }
    
    /** TDD. */
    @Test
    public void addPropertyKONullValue() {
        assertThrows(IllegalArgumentException.class, () ->
            // Given
            testedStore.createProperty(new PropertyString("hi", null)));
        // Then No error
    }
    
    /** TDD. */
    @Test
    public void addPropertyKOInvalidValue() {
        assertThrows(IllegalArgumentException.class, () ->
            // Given
            testedStore.createProperty(new PropertyLogLevel("log", "TRUC")));
        // Then No error
    }
    
    
    // ------------------ read --------------------
    
    @Test
    public void readOK() {
        // Given
        testedStore.createProperty(new PropertyString("toto", "ff4j"));
        // When
        Property<?> ap = testedStore.readProperty("toto");
        // Then
        Assertions.assertNotNull(ap);
        Assertions.assertNotNull(ap.getName());
        Assertions.assertEquals("toto", ap.getName());
        Assertions.assertEquals("ff4j", ap.getValue());
        Assertions.assertEquals("ff4j", ap.asString());
        Assertions.assertNull(ap.getFixedValues());
        
    }
    
    @Test
    public void readOKFixed() {
        // Given
        testedStore.createProperty(new PropertyLogLevel("log", LogLevel.ERROR));
        // When
        Property<?> log = testedStore.readProperty("log");
        // Then
        Assertions.assertNotNull(log);
        Assertions.assertNotNull(log.getName());
        Assertions.assertEquals("log", log.getName());
        Assertions.assertEquals(LogLevel.ERROR, log.getValue());
        Assertions.assertEquals("ERROR", log.asString());
        Assertions.assertNotNull(log.getFixedValues());
    }
    
    /** TDD. */
    @Test
    public void readKOnull() {
        assertThrows(IllegalArgumentException.class, () ->
            // Given
            testedStore.readProperty(null));
    }
    
    /** TDD. */
    @Test
    public void readKOempty() {
        assertThrows(IllegalArgumentException.class, () -> {
            // Given
            testedStore.readProperty("");
            // Expected error
            Assertions.fail();
        });
    }
    
    /** TDD. */
    @Test
    public void readKOnotExist() {
        assertThrows(PropertyNotFoundException.class, () -> {
            // Given
            Assertions.assertFalse(testedStore.existProperty("invalid"));
            // When
            testedStore.readProperty("invalid");
            // Expected error
            Assertions.fail();
        });
    }
    
    // ------------------ update --------------------
    
    /** TDD. */
    @Test
    public void updateKOdoesnotExist() {
        assertThrows(PropertyNotFoundException.class, () -> {
            // Given
            Assertions.assertFalse(testedStore.existProperty("invalid"));
            // When
            testedStore.updateProperty("invalid", "aa");
            // Expected error
            Assertions.fail();
        });
    }
    
    /** TDD. */
    @Test
    public void updateKOdoesnotExist2() {
        assertThrows(PropertyNotFoundException.class, () -> {
            // Given
            Assertions.assertFalse(testedStore.existProperty("invalid"));
            // When
            testedStore.updateProperty(new PropertyString("invalid", "abc"));
            // Expected error
            Assertions.fail();
        });
    }
    
    /** TDD. */
    @Test
    public void updateKOnull() {
        assertThrows(IllegalArgumentException.class, () -> {
            // When
            testedStore.updateProperty(null, "aa");
            // Expected error
            Assertions.fail();
        });
    }
    
    /** TDD. */
    @Test
    public void updateKOempty() {
        assertThrows(IllegalArgumentException.class, () -> {
            // When
            testedStore.updateProperty("", "aa");
            // Expected error
            Assertions.fail();
        });
    }
    
    /** TDD. */
    @Test
    public void updateKoPropertyNull() {
        assertThrows(IllegalArgumentException.class, () -> {
            // When
            testedStore.updateProperty(null);
            // Expected error
            Assertions.fail();
        });
    }
    
    /** TDD. */
    @Test
    public void updateKOInvalidValue() {
        assertThrows(IllegalArgumentException.class, () -> {
            // Given
            testedStore.createProperty(new PropertyLogLevel("log", LogLevel.ERROR));
            // When
            testedStore.updateProperty("log", "KO");
        });
    }
    
    /** TDD. */
    @Test
    public void updateOK() {
        // Given
        testedStore.createProperty(new PropertyLogLevel("log", LogLevel.ERROR));
        // When
        testedStore.updateProperty("log", "INFO");
        // Then
        Assertions.assertEquals(LogLevel.INFO, testedStore.readProperty("log").getValue());
    }
    
    /** TDD. */
    @Test
    public void updateOKProperties() {
        // Given
        testedStore.createProperty(new PropertyLogLevel("log", LogLevel.ERROR));
        // When
        PropertyLogLevel pll = new PropertyLogLevel("log", LogLevel.INFO);
        testedStore.updateProperty(pll);
        // Then
        Assertions.assertEquals(LogLevel.INFO, testedStore.readProperty("log").getValue());
    }
    
    // ------------------ delete -------------------- 

    /** TDD. */
    @Test
    public void deleteKOnull() {
        assertThrows(IllegalArgumentException.class, () -> {
            // When
            testedStore.deleteProperty(null);
            // Expected Error
            Assertions.fail();
        });
    }
    
    /** TDD. */
    @Test
    public void deleteKOempty() {
        assertThrows(IllegalArgumentException.class, () -> {
            // When
            testedStore.deleteProperty("");
            // Expected Error
            Assertions.fail();
        });
    }
    
    /** TDD. */
    @Test
    public void deleteKOdoesnotexist() {
        assertThrows(PropertyNotFoundException.class, () -> {
            // Given
            Assertions.assertFalse(testedStore.existProperty("invalid"));
            // When
            testedStore.deleteProperty("invalid");
            // Expected Error
            Assertions.fail();
        });
    }
    
    /** TDD. */
    @Test
    public void deleteOK() {
        // Given
        testedStore.createProperty(new PropertyString("toto", "ff4j"));
        Assertions.assertTrue(testedStore.existProperty("toto"));
        // When
        testedStore.deleteProperty("toto");
        // Then
        Assertions.assertFalse(testedStore.existProperty("toto"));
    }
    
    @Test
    public void existfilled() {
        // When-Then
        Assertions.assertTrue(testedStore.existProperty("a"));
        Assertions.assertFalse(testedStore.existProperty("k"));
    }
    
    @Test
    public void valueFixed() {
        // When-Then
        Assertions.assertTrue(testedStore.existProperty("a"));
        Assertions.assertEquals("AMER", testedStore.readProperty("a").getValue());
    }
    
    /** TDD. */
    @Test
    public void listPropertyNames() {
        // Given, When
        Set< String > proNames = testedStore.listPropertyNames();
        // Then
       Assertions.assertTrue(proNames.contains("a"));
    }
    
    /** TDD. */
    @Test
    public void readAllProperties() {
        // Given
        Assertions.assertNotNull(testedStore);
        // When
        Map <String, Property<?>> mapsOf = testedStore.readAllProperties();
        // When
        Assertions.assertTrue(mapsOf.containsKey("a"));
        Assertions.assertTrue(mapsOf.containsKey("b"));
    }
    
    /** TDD. */
    @Test
    public void clear() {
        // Given
        Assertions.assertNotNull(testedStore);
        Map <String, Property<?>> before = testedStore.readAllProperties();
        Assertions.assertFalse(before.isEmpty());
        // When
        testedStore.clear();
        // Then
        Assertions.assertTrue(testedStore.readAllProperties().isEmpty());
        
        /// Reinit
        for (String pName : before.keySet()) {
            testedStore.createProperty(before.get(pName));
        }
    }
    
    /** TDD. */
    @Test
    public void importPropertiesNull() {
        // Given
        Assertions.assertNotNull(testedStore);
        // When
        testedStore.importProperties(null);
        // Then, no issue
    }
    
    /** TDD. */
    @Test
    public void importPropertiesOK() {
        // Given
        Assertions.assertNotNull(testedStore);
        Assertions.assertFalse(testedStore.existProperty("titi1"));
        Assertions.assertFalse(testedStore.existProperty("titi2"));
        Assertions.assertTrue(testedStore.existProperty("a"));
        
        // When
        Set < Property<?>> setOfProperty = new HashSet<Property<?>>();
        setOfProperty.add(new PropertyLogLevel("a", LogLevel.INFO));
        setOfProperty.add(new PropertyLogLevel("titi1", LogLevel.INFO));
        setOfProperty.add(new PropertyLogLevel("titi2", LogLevel.INFO));
        testedStore.importProperties(setOfProperty);
        
        // Then
        Assertions.assertTrue(testedStore.existProperty("titi1"));
        Assertions.assertTrue(testedStore.existProperty("titi2"));
        Assertions.assertTrue(testedStore.existProperty("a"));
    }
    
    /** TDD. */
    @Test
    public void readPropertyDefaultExist() {
        // Given
        Assertions.assertTrue(testedStore.existProperty("a"));
        // When
        Property<?> defaultA = new PropertyString("a", "GLOUGLOU");
        // Then
        Assertions.assertEquals("AMER", testedStore.readProperty("a", defaultA).getValue());
    }
    
    /** TDD. */
    @Test
    public void readPropertyDefaultNotExist() {
        Property<?> defaultA = new PropertyString("aaaa", "GLOUGLOU");
        // Given
        Assertions.assertFalse(testedStore.existProperty("aaaa"));
        // Then
        Assertions.assertEquals("GLOUGLOU", testedStore.readProperty("aaaa", defaultA).getValue());
    }
    
}
