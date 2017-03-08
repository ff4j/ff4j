package org.ff4j.archaius;

/*
 * #%L
 * ff4j-archaius
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

import org.ff4j.property.Property;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import com.netflix.config.AbstractPollingScheduler;
import com.netflix.config.DynamicConfiguration;
import com.netflix.config.FixedDelayPollingScheduler;
import com.netflix.config.PolledConfigurationSource;

/**
 * Test Archiaus property store (with FF4JConfigurationSource)
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyStoreArchaiusPolledSourceTest extends PropertyStoreTestSupport {
    
    private static DynamicConfiguration configuration = null;
            
    @BeforeClass
    public static void initArchauis() throws InterruptedException {
     // Sample FF4J Store
        PropertyStore ff4jStore = new InMemoryPropertyStore("ff4j-properties.xml");
        // FF4Store as polling Source for Archiaus
        PolledConfigurationSource ff4jSource = new FF4jPolledConfigurationSource(ff4jStore);
        // Working Thread (polling from ff4j => Archaius)
        AbstractPollingScheduler scheduler = new FixedDelayPollingScheduler(0, 1000, true);
        // Define configuration with polling and source
        configuration = new DynamicConfiguration(ff4jSource,scheduler);
    }
    
    @Test(expected = IllegalStateException.class)
    public void testPollInvalid() throws Exception {
        PolledConfigurationSource ff4jSource = new FF4jPolledConfigurationSource();
        ff4jSource.poll(true, null);
    }
    
    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        return new PropertyStoreArchaius(configuration);
    }
    
    /** TDD. */
    @Override
    @Test
    @Ignore
    public void updateKOInvalidValue() {
        // Cannot through error as FixedValue are not in Commons-config.
        System.out.println("Not Supported as fixedValues are ignored");
    }
    
    /** TDD. */
    @Override
    @Test
    @Ignore
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
        Assert.assertNotNull(log);
        Assert.assertNotNull(log.getName());
        Assert.assertEquals(READ_OK_FIXED, log.getName());
        Assert.assertEquals(LogLevel.ERROR.name(), log.getValue());
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
        Assert.assertEquals("INFO", testedStore.readProperty(UPDATE_OK).getValue());
    }
    

}
