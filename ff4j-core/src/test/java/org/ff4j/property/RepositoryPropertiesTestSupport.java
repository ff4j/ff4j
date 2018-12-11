package org.ff4j.property;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Map;
import java.util.stream.Stream;

import org.ff4j.FF4j;
import org.ff4j.exception.AssertionViolationException;
import org.ff4j.parser.ConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.ff4j.property.exception.PropertyNotFoundException;
import org.ff4j.property.repository.PropertyRepository;
import org.ff4j.test.AssertFF4j;
import org.ff4j.test.FF4jTestDataSet;
import org.ff4j.utils.Util;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

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

/**
 * SuperClass to test stores within core project
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class RepositoryPropertiesTestSupport implements FF4jTestDataSet {
    
    /** Initialize */
    protected FF4j ff4j = null;

    /** Tested Store. */
    protected PropertyRepository testedStore;

    /** Test Values */
    protected AssertFF4j assertFF4j;
    
    /** DataSet. **/
    protected FF4jConfigFile testDataSet;
    
    /** {@inheritDoc} */
    @BeforeEach
    public void setUp() throws Exception {
        ConfigurationFileParser.clearCache();
        ff4j        = new FF4j().withRepositoryProperties(initStore());
        assertFF4j  = new AssertFF4j(ff4j);
        testedStore = ff4j.getRepositoryProperties();
        testDataSet = expectConfig();
    }
    
    /**
     * Any store test will declare its store through this callback.
     * 
     * @return working feature store
     * @throws Exception
     *          Hi guys, just let you know I did the update in the presentation : changing instructors names to put the 2 of you    error during building feature store
     */
    protected abstract PropertyRepository initStore();

    // -- exists --
    
    @Test
    @DisplayName("When test property existence with null param, expecting AssertionViolationException")
    public void existWithNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.exists(null); });
        assertThrows(AssertionViolationException.class, () -> { testedStore.exists(""); });
    }
    
    // -- create --
    
    @Test
    @DisplayName("When creating properties with null param, expecting AssertionViolationException")
    public void createWithNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.save((Property<?>)null); });
    }
   
    @Test
    @DisplayName("When creating new property, it becomes available")
    public void createShouldMakeNewPropertyAvailable() throws Exception {
        assertFF4j.assertThatPropertyStoreHasSize(testDataSet.getProperties().size());
        assertFF4j.assertThatPropertyDoesNotExist(PROPERTY_FOR_TEST);
        PropertyString p = new PropertyString(PROPERTY_FOR_TEST);
        p.setValue("v1").description("ok");
        testedStore.save(p);
        assertFF4j.assertThatPropertyStoreHasSize(testDataSet.getProperties().size() + 1);
        assertFF4j.assertThatPropertyExist(PROPERTY_FOR_TEST);
    }
    
    @Test
    @DisplayName("When creating new property, it becomes available")
    public void createShouldMakeNewPropertyLogLevelAvailable() throws Exception {
        assertFF4j.assertThatPropertyStoreHasSize(testDataSet.getProperties().size());
        assertFF4j.assertThatPropertyDoesNotExist(PROPERTY_FOR_TEST);
        PropertyLogLevel p = new PropertyLogLevel(PROPERTY_FOR_TEST, LogLevel.INFO);
        testedStore.save(p);
        assertFF4j.assertThatPropertyStoreHasSize(testDataSet.getProperties().size() + 1);
        assertFF4j.assertThatPropertyExist(PROPERTY_FOR_TEST);
    }  
    
    // -- read --
    
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
        assertFF4j.assertThatPropertyStoreHasSize(testDataSet.getProperties().size());
        // When
        Map < String, Property<?> > properties = Util.toMap(testedStore.findAll());
        // Then
        Assertions.assertEquals(testDataSet.getProperties().size(), properties.size());
    } 
    
    // -- update --
    
    @Test
    @DisplayName("When updating property with null param, expecting AssertionViolationException")
    public void updatePropertyWithNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { 
            testedStore.save((Property<?>) null); 
        });
    }
    
    @Test
    @DisplayName("When updating property with unknowm param, creating the property")
    public void updatedUnknownPropertyShouldThrowPropertyNotFound() throws Exception {
        assertFF4j.assertThatPropertyDoesNotExist(PROPERTY_FOR_TEST);
        testedStore.save(new PropertyString(PROPERTY_FOR_TEST, "OK"));
        assertFF4j.assertThatPropertyExist(PROPERTY_FOR_TEST);
    }
    
    @Test
    @DisplayName("When updating property, all attributes should be updated")
    public void testUpdatePropertiesAll() {
        // Givens
        String newDescription = "new-description";
        assertFF4j.assertThatPropertyExist(PDouble);
        Assertions.assertFalse(newDescription.equals(testedStore.read(PDouble).getDescription().isPresent()));
        Assertions.assertFalse(testedStore.read(PDouble).getFixedValues().isPresent());
        // When
        PropertyDouble propDouble = (PropertyDouble) testedStore.read(PDouble);
        propDouble.setDescription(newDescription);
        propDouble.add2FixedValueFromString("20.0");
        propDouble.add2FixedValueFromString("10.0");
        propDouble.add2FixedValueFromString("30.0");
        testedStore.save(propDouble);
        // Then
        PropertyDouble updatedProp = (PropertyDouble) testedStore.read(PDouble);
        Assertions.assertTrue(newDescription.equals(updatedProp.getDescription().get()));
        Assertions.assertTrue(updatedProp.getFixedValues().isPresent());
        Assertions.assertEquals(3, testedStore.read(PDouble).getFixedValues().get().size());
    }
    
    // -- delete --
    
    @Test
    @DisplayName("When deleting new property, it is not available anymore")
    public void deleteShouldRemovePropertyAvailable() throws Exception {
        assertFF4j.assertThatPropertyStoreHasSize(testDataSet.getProperties().size());
        assertFF4j.assertThatPropertyExist(PDouble);
        testedStore.delete(PDouble);
        assertFF4j.assertThatPropertyStoreHasSize(testDataSet.getProperties().size() - 1 );
        assertFF4j.assertThatPropertyDoesNotExist(PDouble);
    }
    
    @Test
    @DisplayName("When deleting property with null param, expecting AssertionViolationException")
    public void deletePropertyWithNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.deleteProperty((String) null); });
    }
    
    @Test
    @DisplayName("When deleting property with null param, expecting PropertyNotFoundException")
    public void deleteUnknownPropertyShouldThrowPropertyNotFound() throws Exception {
        assertFF4j.assertThatPropertyDoesNotExist(PROPERTY_FOR_TEST);
        assertThrows(PropertyNotFoundException.class, () -> { 
            testedStore.delete(PROPERTY_FOR_TEST); 
        });
    }
    
    @Test
    @DisplayName("When invoking clear all properties are deleted")
    public void clearShouldEmptyRepository() {
        assertFF4j.assertThatPropertyExist(PDouble);
        assertFF4j.assertThatPropertyExist(PInstant);
        assertFF4j.assertThatPropertyExist(PInt);
        assertFF4j.assertThatPropertyExist(PDate);
        testedStore.deleteAll();
        // Then
        Assertions.assertEquals(0, testedStore.findAll().count());
    }
    
    @Test
    @DisplayName("When listing all properties names, the are all retrieve")
    public void listingPropertyNamesShouldRetrieveAllNames() {
        Stream< String> propertyNames = testedStore.listPropertyNames();
        Assertions.assertNotNull(propertyNames);
        Assertions.assertEquals(testDataSet.getProperties().size(), propertyNames.count());
    }
        
}
