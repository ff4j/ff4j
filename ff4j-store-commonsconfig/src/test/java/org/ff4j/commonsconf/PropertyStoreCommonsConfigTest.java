package org.ff4j.commonsconf;

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

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Initialize store for commons config.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyStoreCommonsConfigTest extends PropertyStoreTestSupport {
    
    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        Configuration conf = null;
        try {
            conf = new PropertiesConfiguration("application.properties");
        } catch (ConfigurationException e) {}
        return new PropertyStoreCommonsConfig(conf);
    }
    
    @Test
    public void initializationOK() {
        PropertyStoreCommonsConfig psConf = new PropertyStoreCommonsConfig();
        psConf.setConfiguration(new PropertiesConfiguration());
        Assert.assertNotNull(psConf.getConfiguration());
    }
    
    @Test(expected = IllegalStateException.class)
    public void initializationKO() {
        PropertyStoreCommonsConfig psConf = new PropertyStoreCommonsConfig();
        psConf.existProperty("toto");
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
