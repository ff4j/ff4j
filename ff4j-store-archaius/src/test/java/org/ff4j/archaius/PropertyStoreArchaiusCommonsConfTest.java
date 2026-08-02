package org.ff4j.archaius;

/*-
 * #%L
 * ff4j-store-archaius
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

import org.ff4j.commonsconf.FF4jConfiguration;
import org.ff4j.commonsconf.PropertyStoreCommonsConfig;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/**
 * Initialized Archaius with a commons config (no dynamic config)
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyStoreArchaiusCommonsConfTest extends PropertyStoreTestSupport {
    
    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStore ff4jStore = new InMemoryPropertyStore("ff4j-properties.xml");
        return new PropertyStoreArchaius(new FF4jConfiguration(ff4jStore));
    }
    
    /** {@inheritDoc} */
    @Test
    public void testInitPropertyStore2() {
        PropertyStore sourceStore   = new InMemoryPropertyStore("ff4j-properties.xml");
        PropertyStore archaiusStore = new PropertyStoreArchaius(sourceStore);
        Assertions.assertTrue(archaiusStore.existProperty("a"));
    }

    @Test
    public void initializationKO() {
        assertThrows(IllegalStateException.class, () -> {
            PropertyStoreCommonsConfig psConf = new PropertyStoreCommonsConfig();
            psConf.existProperty("toto");
        });
    }
    
    /** TDD. */
    @Override
    @Test
    @Disabled
    public void updateKOInvalidValue() {
        // Cannot through error as FixedValue are not in Commons-config.
        System.out.println("Not Supported as fixedValues are ignored");
    }
    
    /** TDD. */
    @Override
    @Test
    @Disabled
    public void updateOKProperties() {
        System.out.println("Not Supported as all properties are String");
    }
    
    /** TDD. */
    @Override
    @Test
    public void readOKFixed() {
      // Given
        testedStore.createProperty(new PropertyLogLevel(READ_OK_FIXED, LogLevel.ERROR));
        // When
        Property<?> log = testedStore.readProperty(READ_OK_FIXED);
        // Then
        Assertions.assertNotNull(log);
        Assertions.assertNotNull(log.getName());
        Assertions.assertEquals(READ_OK_FIXED, log.getName());
        Assertions.assertEquals(LogLevel.ERROR.name(), log.getValue());
    }
    
    /** TDD. */
    @Override
    @Test
    public void updateOK() {
        // Given
        testedStore.createProperty(new PropertyLogLevel(UPDATE_OK, LogLevel.ERROR));
        // When
        testedStore.updateProperty(UPDATE_OK, "INFO");
        // Then
        Assertions.assertEquals("INFO", testedStore.readProperty(UPDATE_OK).getValue());
    }
    
}
