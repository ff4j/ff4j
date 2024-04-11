package org.ff4j.awsssm.store;

/*-
 * #%L
 * ff4j-store-aws-ssm
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

import java.util.Map;

import org.ff4j.property.Property;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;


@Ignore
public class PropertyStoreAwsSSMTest extends PropertyStoreTestSupport {

    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
        testedStore.createProperty(new PropertyString("a", "AMER"));
        testedStore.createProperty(new PropertyString("b", "12"));
    }

    @After
    public void tearDown() {
        testedStore.clear();
    }

    @Override
     protected PropertyStore initPropertyStore() {
        return new PropertyStoreAwsSSM("/Dev/ff4j");
    }

    @Test(expected = IllegalArgumentException.class)
    public void newStoreBadPathBegin() {
        // Given
        new PropertyStoreAwsSSM("Toto");
        // When, Then
        // Expect error
    }

    @Test(expected = IllegalArgumentException.class)
    public void newStoreBadPathEnd() {
        // Given
        new PropertyStoreAwsSSM("/Toto/");
        // When, Then
        // Expect error
    }

    @Override
    @Test
    public void readOKFixed() {
        // Given
        testedStore.createProperty(new PropertyLogLevel(READ_OK_FIXED, PropertyLogLevel.LogLevel.ERROR));
        // When
        Property<?> log = testedStore.readProperty(READ_OK_FIXED);
        // Then
        Assert.assertNotNull(log);
        Assert.assertNotNull(log.getName());
        Assert.assertEquals(READ_OK_FIXED, log.getName());
        Assert.assertEquals(PropertyLogLevel.LogLevel.ERROR.name(), log.getValue());
    }

    @Override
    @Test
    @Ignore
    public void updateKOInvalidValue() {
        // Cannot through error as FixedValue are not in Commons-config.
        System.out.println("Not Supported as fixedValues are ignored");
    }

    @Override
    @Test
    @Ignore
    public void updateOKProperties() {
        System.out.println("Not Supported as all properties are String");
    }

    @Override
    @Test
    public void updateOK() {
        // Given
        testedStore.createProperty(new PropertyLogLevel(UPDATE_OK, PropertyLogLevel.LogLevel.ERROR));
        // When
        testedStore.updateProperty(UPDATE_OK, "INFO");
        // Then
        Assert.assertEquals("INFO", testedStore.readProperty(UPDATE_OK).getValue());
    }

    @Override
    @Test
    public void clear() {
        // Given
        Map<String, Property<?>> before = testedStore.readAllProperties();
        Assert.assertFalse(before.isEmpty());
        // When
        testedStore.clear();
        // Then
        Assert.assertTrue(testedStore.readAllProperties().isEmpty());
    }
}
