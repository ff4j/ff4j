package org.ff4j.awsssm.store;

import org.ff4j.property.Property;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.*;

import java.util.Map;


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
