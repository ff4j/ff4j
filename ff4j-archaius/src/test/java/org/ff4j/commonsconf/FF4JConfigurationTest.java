package org.ff4j.commonsconf;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

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

import java.util.Properties;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.ff4j.exception.InvalidPropertyTypeException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Test implementatiton of {@link Configuration} relying on FF4J {@link PropertyStore}.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FF4JConfigurationTest {
    
    /** Configuration implementation relying on property store. */
    private FF4jConfiguration ff4jConf;
    
    /** Underlying ff4j property store. */
    private PropertyStore pStore;
            
    @Before
    public void initCommonsConfWithFF4j() throws ConfigurationException {
        pStore = new InMemoryPropertyStore("ff4j-configuration.xml");
        ff4jConf = new FF4jConfiguration();
        ff4jConf.setFf4jStore(pStore);
    }
    
    @Test
    public void testSubSet() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("name"));
        // When
        Configuration conf2 = ff4jConf.subset("prop");
        // Then
        Assert.assertTrue(conf2.containsKey("propInt"));
        Assert.assertFalse(conf2.containsKey("name"));
    }
    
    @Test
    public void testgetPropertiesByKey() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("name"));
        // When
        Properties conf2 = ff4jConf.getProperties("prop");
        // Then
        Assert.assertTrue(conf2.containsKey("propInt"));
        Assert.assertFalse(conf2.containsKey("name"));
        Assert.assertTrue(ff4jConf.getProperties(null).isEmpty());
    }
    
    @Test
    public void testNullValues() {
        Assert.assertFalse(ff4jConf.containsKey(null));
        Assert.assertFalse(ff4jConf.isEmpty());
    }
    
    @Test
    public void testAddProperty() {
        // Given
        Assert.assertFalse(ff4jConf.containsKey("myNewProp"));
        // When
        ff4jConf.addProperty("myNewProp", "hello");
        // Then
        Assert.assertTrue(ff4jConf.containsKey("myNewProp"));
        Assert.assertTrue(pStore.existProperty("myNewProp"));
        Assert.assertEquals("hello", pStore.readProperty("myNewProp").asString());
    }
    
    @Test
    public void testAddPropertyDirect() {
        // Given
        Assert.assertFalse(ff4jConf.containsKey("myNewProp"));
        // When
        ff4jConf.addPropertyDirect("myNewProp", "hello");
        // Then
        Assert.assertTrue(ff4jConf.containsKey("myNewProp"));
        Assert.assertTrue(pStore.existProperty("myNewProp"));
        Assert.assertEquals("hello", pStore.readProperty("myNewProp").asString());
    }
    
    @Test
    public void testSetPropertyOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        Assert.assertNotEquals(10, ff4jConf.getInt("propInt"));
        // When
        ff4jConf.setProperty("propInt", 10);
        // Then
        Assert.assertEquals(10, ff4jConf.getInt("propInt"));
    }
    
    @Test(expected = InvalidPropertyTypeException.class)
    public void testSetPropertyKO() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        Assert.assertNotEquals(10, ff4jConf.getInt("propInt"));
        // When
        ff4jConf.setProperty("propInt", "hello");
        // Then
        Assert.assertEquals(10, ff4jConf.getInt("propInt"));
    }
    
    @Test
    public void testClearProperty() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        // When
        ff4jConf.clearProperty("propInt");
        // Then
        Assert.assertFalse(ff4jConf.containsKey("propInt"));
        Assert.assertFalse(pStore.existProperty("myNewProp"));
    }
    
    @Test
    public void testClear() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        // When
        ff4jConf.clear();
        // Then
        Assert.assertFalse(ff4jConf.containsKey("propInt"));
        Assert.assertTrue(ff4jConf.isEmpty());
        Assert.assertTrue(pStore.isEmpty());
    }
    
    @Test
    public void testgetKeys() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        Assert.assertTrue(ff4jConf.containsKey("propString"));
        Assert.assertTrue(ff4jConf.getKeys().hasNext());
        // When
        Iterator<String> propsKeys = ff4jConf.getKeys("prop");
        // Then
        Assert.assertTrue(propsKeys.hasNext());
    }
    
    @Test
    public void testgetKeysEmpty() {
        // Given
        Assert.assertFalse(ff4jConf.containsKey("z"));
        // When
        Iterator<String> propsKeys = ff4jConf.getKeys("z");
        // Then
        Assert.assertFalse(propsKeys.hasNext());
    }
    
    @Test
    public void testgetKeysNull() {
        // Given
        InMemoryPropertyStore tmpStore = new InMemoryPropertyStore();
        Configuration tmpConf  = new FF4jConfiguration(tmpStore);
        Iterator<String> propsKeys = tmpConf.getKeys("z");
        Assert.assertFalse(propsKeys.hasNext());
        
        tmpStore.setProperties(null);
        Assert.assertFalse(tmpConf.getKeys().hasNext());
        Assert.assertFalse(tmpConf.getKeys("z").hasNext());
    }
    
    @Test(expected = InvalidPropertyTypeException.class)
    public void testgetBooleanKO() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        // When
        ff4jConf.getBoolean("propInt");
    }
    
    @Test
    public void testgetBooleanOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propBool"));
        Assert.assertTrue(ff4jConf.containsKey("propBool2"));
        // When
        Assert.assertEquals("true", ff4jConf.getString("propBool"));
        Assert.assertEquals("false", ff4jConf.getString("propBool2"));
        Assert.assertTrue(ff4jConf.getBoolean("propBool"));
        Assert.assertFalse(ff4jConf.getBoolean("propBool2"));
        Assert.assertEquals(Boolean.TRUE, ff4jConf.getBoolean("propBool"));
    }
    
    @Test
    public void testgetBooleanDefault() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propBool"));
        // When
        Assert.assertEquals("true", ff4jConf.getString("propBool"));
        Assert.assertTrue(ff4jConf.getBoolean("propBool", false));
        Assert.assertFalse(ff4jConf.getBoolean("invalid", false));
        Assert.assertEquals(Boolean.TRUE, ff4jConf.getBoolean("propBool", Boolean.FALSE));
        Assert.assertEquals(Boolean.FALSE, ff4jConf.getBoolean("invalid", Boolean.FALSE));
    }

    @Test(expected = InvalidPropertyTypeException.class)
    public void testgetByteKO() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propString"));
        // When
        ff4jConf.getByte("propString");
    }
    
    @Test
    public void testgetByteOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propByte"));
        // When
        Assert.assertEquals("123", ff4jConf.getString("propByte"));
        Assert.assertEquals(new Byte("123"), (Byte) ff4jConf.getByte("propByte"));
        Assert.assertEquals(new Byte("123").byteValue(), ff4jConf.getByte("propByte"));
        Assert.assertEquals(123, ff4jConf.getByte("propByte"));
    }
    
    @Test
    public void testgetByteDefault() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propByte"));
        // When
        Assert.assertEquals("123", ff4jConf.getString("propByte"));
        Assert.assertEquals(new Byte("120"), (Byte) ff4jConf.getByte("invalid", new Byte("120")));
        Assert.assertEquals(new Byte("120").byteValue(), ff4jConf.getByte("invalid", new Byte("120").byteValue()));
        Assert.assertEquals(new Byte("123"), (Byte) ff4jConf.getByte("propByte", new Byte("120")));
        Assert.assertEquals(new Byte("123").byteValue(), ff4jConf.getByte("propByte", new Byte("120").byteValue()));
    }
    
    @Test(expected = InvalidPropertyTypeException.class)
    public void testgetDoubleKO() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propString"));
        // When
        ff4jConf.getDouble("propString");
    }
    
    @Test
    public void testgetDoubleOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propDouble"));
        // When
        Assert.assertEquals(new Double(12.5), (Double) ff4jConf.getDouble("propDouble"));
        Assert.assertEquals(new Double(12.5), new Double(ff4jConf.getDouble("propDouble")));
    }
    
    @Test
    public void testgetDoubleDefault() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propDouble"));
        Assert.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assert.assertEquals(new Double(12.5), (Double) ff4jConf.getDouble("propDouble", 20.5));
        Assert.assertEquals(new Double(20.5), (Double) ff4jConf.getDouble("invalid", 20.5));
        Assert.assertEquals(new Double(12.5), (Double) ff4jConf.getDouble("propDouble", new Double(20.5)));
        Assert.assertEquals(new Double(20.5), (Double) ff4jConf.getDouble("invalid", new Double(20.5)));
    }
    
    @Test(expected = InvalidPropertyTypeException.class)
    public void testgetFloatKO() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propString"));
        // When
        ff4jConf.getFloat("propString");
    }
    
    @Test
    public void testgetFloatOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propDouble"));
        // When
        Assert.assertEquals(new Float(12.5), (Float) ff4jConf.getFloat("propDouble"));
        Assert.assertEquals(new Float(12.5), new Float(ff4jConf.getFloat("propDouble")));
    }
    
    @Test
    public void testgetFloatDefault() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propDouble"));
        Assert.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assert.assertEquals(new Float(12.5), (Float) ff4jConf.getFloat("propDouble", 20.5f));
        Assert.assertEquals(new Float(20.5), (Float) ff4jConf.getFloat("invalid", 20.5f));
        Assert.assertEquals(new Float(12.5), (Float) ff4jConf.getFloat("propDouble", new Float(20.5)));
        Assert.assertEquals(new Float(20.5), (Float) ff4jConf.getFloat("invalid", new Float(20.5)));
    }
    
    @Test(expected = InvalidPropertyTypeException.class)
    public void testgetIntKO() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propString"));
        // When
        ff4jConf.getInt("propString");
    }
    
    @Test
    public void testgetIntOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        // When
        Assert.assertEquals(new Integer(12), (Integer) ff4jConf.getInt("propInt"));
        Assert.assertEquals(new Integer(12), new Integer(ff4jConf.getInt("propInt")));
    }
    
    @Test
    public void testgetIntDefault() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        Assert.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assert.assertEquals(new Integer(12), (Integer) ff4jConf.getInt("propInt", 20));
        Assert.assertEquals(new Integer(20), (Integer) ff4jConf.getInt("invalid", 20));
        Assert.assertEquals(new Integer(12), (Integer) ff4jConf.getInteger("propInt", new Integer(20)));
        Assert.assertEquals(new Integer(20), (Integer) ff4jConf.getInteger("invalid", new Integer(20)));
    }
    
    @Test(expected = InvalidPropertyTypeException.class)
    public void testgetLongKO() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propString"));
        // When
        ff4jConf.getLong("propString");
    }
    
    @Test
    public void testgetLongOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propLong"));
        // When
        Assert.assertEquals(new Long(12), (Long) ff4jConf.getLong("propLong"));
        Assert.assertEquals(new Long(12), new Long(ff4jConf.getLong("propLong")));
    }
    
    @Test
    public void testgetLongDefault() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        Assert.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assert.assertEquals(new Long(12), (Long) ff4jConf.getLong("propLong", 20));
        Assert.assertEquals(new Long(20), (Long) ff4jConf.getLong("invalid", 20));
        Assert.assertEquals(new Long(12), (Long) ff4jConf.getLong("propLong", new Long(20)));
        Assert.assertEquals(new Long(20), (Long) ff4jConf.getLong("invalid", new Long(20)));
    }
    
    @Test(expected = InvalidPropertyTypeException.class)
    public void testgetShortKO() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propString"));
        // When
        ff4jConf.getShort("propString");
    }
    
    @Test
    public void testgetShortOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propShort"));
        // When
        Assert.assertEquals(new Short("12"), (Short) ff4jConf.getShort("propShort"));
        Assert.assertEquals(new Short("12"), new Short(ff4jConf.getShort("propShort")));
    }
    
    @Test
    public void testgetShortDefault() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        Assert.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assert.assertEquals(new Short("12"), (Short) ff4jConf.getShort("propShort", new Short("20").shortValue()));
        Assert.assertEquals(new Short("20"), (Short) ff4jConf.getShort("invalid", new Short("20").shortValue()));
        Assert.assertEquals(new Short("12"), (Short) ff4jConf.getShort("propShort", new Short("20")));
        Assert.assertEquals(new Short("20"), (Short) ff4jConf.getShort("invalid", new Short("20")));
    }
    
    @Test(expected = InvalidPropertyTypeException.class)
    public void testgetBigDecimalKO() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propString"));
        // When
        ff4jConf.getBigDecimal("propString");
    }
    
    @Test
    public void testgetBigDecimalOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propDouble"));
        // When
        Assert.assertEquals(new BigDecimal(12.5), (BigDecimal) ff4jConf.getBigDecimal("propDouble"));
    }
    
    @Test
    public void testgetBigDecimalDefault() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propDouble"));
        Assert.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assert.assertEquals(new BigDecimal(12.5), (BigDecimal) ff4jConf.getBigDecimal("propDouble", new BigDecimal(20.5)));
        Assert.assertEquals(new BigDecimal(20.5), (BigDecimal) ff4jConf.getBigDecimal("invalid", new BigDecimal(20.5)));
    }
    
    @Test(expected = InvalidPropertyTypeException.class)
    public void testgetBigIntegerKO() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propString"));
        // When
        ff4jConf.getBigInteger("propString");
    }
    
    @Test
    public void testgetBigIntegerOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        // When
        Assert.assertEquals(new BigInteger("12"), (BigInteger) ff4jConf.getBigInteger("propInt"));
        Assert.assertEquals(new BigInteger("12"), new BigInteger(ff4jConf.getString("propInt")));
    }
    
    @Test
    public void testgetBigIntegerDefault() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propInt"));
        Assert.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assert.assertEquals(new BigInteger("12"), (BigInteger) ff4jConf.getBigInteger("propInt", new BigInteger("20")));
        Assert.assertEquals(new BigInteger("20"), (BigInteger) ff4jConf.getBigInteger("invalid", new BigInteger("20")));
    }
    
    @Test
    public void testgetStringDefault() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propString"));
        Assert.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assert.assertEquals("hello", ff4jConf.getString("propString", "byebye"));
        Assert.assertEquals("byebye", ff4jConf.getString("invalid", "byebye"));
    }
    
    @Test(expected = PropertyNotFoundException.class)
    public void getListInvalid() {
        Assert.assertFalse(ff4jConf.containsKey("toto"));
        ff4jConf.getList("toto");
    }
    
    @Test
    public void getListOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propList"));
        // When
        List<Object> c = ff4jConf.getList("propList");
        // Then
        Assert.assertTrue(Util.list("a","b","c").containsAll(c));
        Assert.assertNotNull(ff4jConf.getList("propEmptyList"));
    }
    
    @Test
    public void getListArrayOK() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propList"));
        // When
        String[] c = ff4jConf.getStringArray("propList");
        // Then
        Assert.assertTrue(Util.list("a","b","c").containsAll(Arrays.asList(c)));
    }
    
    @Test
    public void testgetListDefault() {
        // Given
        Assert.assertTrue(ff4jConf.containsKey("propList"));
        Assert.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        List<Object> c1 = ff4jConf.getList("propList", Util.list("d","e","f"));
        List<Object> c2 = ff4jConf.getList("invalid",  Util.list("d","e","f"));
        // Then
        Assert.assertTrue(Util.list("a","b","c").containsAll(c1));
        Assert.assertTrue(Util.list("d","e","f").containsAll(c2));
    }
    
    
    @Test(expected = IllegalStateException.class)
    public void testNullStore() {
        new FF4jConfiguration().ff4jStore();
    }
    
}
