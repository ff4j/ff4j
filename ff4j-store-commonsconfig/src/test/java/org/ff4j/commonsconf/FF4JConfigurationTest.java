package org.ff4j.commonsconf;

/*-
 * #%L
 * ff4j-store-commonsconfig
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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import java.util.Properties;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.ff4j.exception.InvalidPropertyTypeException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

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
            
    @BeforeEach
    public void initCommonsConfWithFF4j() throws ConfigurationException {
        pStore = new InMemoryPropertyStore("ff4j-configuration.xml");
        ff4jConf = new FF4jConfiguration();
        ff4jConf.setFf4jStore(pStore);
    }
    
    @Test
    public void testSubSet() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("name"));
        // When
        Configuration conf2 = ff4jConf.subset("prop");
        // Then
        Assertions.assertTrue(conf2.containsKey("propInt"));
        Assertions.assertFalse(conf2.containsKey("name"));
    }
    
    @Test
    public void testgetPropertiesByKey() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("name"));
        // When
        Properties conf2 = ff4jConf.getProperties("prop");
        // Then
        Assertions.assertTrue(conf2.containsKey("propInt"));
        Assertions.assertFalse(conf2.containsKey("name"));
        Assertions.assertTrue(ff4jConf.getProperties(null).isEmpty());
    }
    
    @Test
    public void testNullValues() {
        Assertions.assertFalse(ff4jConf.containsKey(null));
        Assertions.assertFalse(ff4jConf.isEmpty());
    }
    
    @Test
    public void testAddProperty() {
        // Given
        Assertions.assertFalse(ff4jConf.containsKey("myNewProp"));
        // When
        ff4jConf.addProperty("myNewProp", "hello");
        // Then
        Assertions.assertTrue(ff4jConf.containsKey("myNewProp"));
        Assertions.assertTrue(pStore.existProperty("myNewProp"));
        Assertions.assertEquals("hello", pStore.readProperty("myNewProp").asString());
    }
    
    @Test
    public void testAddPropertyDirect() {
        // Given
        Assertions.assertFalse(ff4jConf.containsKey("myNewProp"));
        // When
        ff4jConf.addPropertyDirect("myNewProp", "hello");
        // Then
        Assertions.assertTrue(ff4jConf.containsKey("myNewProp"));
        Assertions.assertTrue(pStore.existProperty("myNewProp"));
        Assertions.assertEquals("hello", pStore.readProperty("myNewProp").asString());
    }
    
    @Test
    public void testSetPropertyOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propInt"));
        Assertions.assertNotEquals(10, ff4jConf.getInt("propInt"));
        // When
        ff4jConf.setProperty("propInt", 10);
        // Then
        Assertions.assertEquals(10, ff4jConf.getInt("propInt"));
    }
    
    @Test
    public void testSetPropertyKO() {
        assertThrows(InvalidPropertyTypeException.class, () -> {
            // Given
            Assertions.assertTrue(ff4jConf.containsKey("propInt"));
            Assertions.assertNotEquals(10, ff4jConf.getInt("propInt"));
            // When
            ff4jConf.setProperty("propInt", "hello");
            // Then
            Assertions.assertEquals(10, ff4jConf.getInt("propInt"));
        });
    }
    
    @Test
    public void testClearProperty() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propInt"));
        // When
        ff4jConf.clearProperty("propInt");
        // Then
        Assertions.assertFalse(ff4jConf.containsKey("propInt"));
        Assertions.assertFalse(pStore.existProperty("myNewProp"));
    }
    
    @Test
    public void testClear() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propInt"));
        // When
        ff4jConf.clear();
        // Then
        Assertions.assertFalse(ff4jConf.containsKey("propInt"));
        Assertions.assertTrue(ff4jConf.isEmpty());
        Assertions.assertTrue(pStore.isEmpty());
    }
    
    @Test
    public void testgetKeys() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propInt"));
        Assertions.assertTrue(ff4jConf.containsKey("propString"));
        Assertions.assertTrue(ff4jConf.getKeys().hasNext());
        // When
        Iterator<String> propsKeys = ff4jConf.getKeys("prop");
        // Then
        Assertions.assertTrue(propsKeys.hasNext());
    }
    
    @Test
    public void testgetKeysEmpty() {
        // Given
        Assertions.assertFalse(ff4jConf.containsKey("z"));
        // When
        Iterator<String> propsKeys = ff4jConf.getKeys("z");
        // Then
        Assertions.assertFalse(propsKeys.hasNext());
    }
    
    @Test
    public void testgetKeysNull() {
        // Given
        InMemoryPropertyStore tmpStore = new InMemoryPropertyStore();
        Configuration tmpConf  = new FF4jConfiguration(tmpStore);
        Iterator<String> propsKeys = tmpConf.getKeys("z");
        Assertions.assertFalse(propsKeys.hasNext());
        
        tmpStore.setProperties(null);
        Assertions.assertFalse(tmpConf.getKeys().hasNext());
        Assertions.assertFalse(tmpConf.getKeys("z").hasNext());
    }
    
    @Test
    public void testgetBooleanKO() {
        assertThrows(InvalidPropertyTypeException.class, () -> {
            // Given
            Assertions.assertTrue(ff4jConf.containsKey("propInt"));
            // When
            ff4jConf.getBoolean("propInt");
        });
    }
    
    @Test
    public void testgetBooleanOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propBool"));
        Assertions.assertTrue(ff4jConf.containsKey("propBool2"));
        // When
        Assertions.assertEquals("true", ff4jConf.getString("propBool"));
        Assertions.assertEquals("false", ff4jConf.getString("propBool2"));
        Assertions.assertTrue(ff4jConf.getBoolean("propBool"));
        Assertions.assertFalse(ff4jConf.getBoolean("propBool2"));
        Assertions.assertEquals(Boolean.TRUE, ff4jConf.getBoolean("propBool"));
    }
    
    @Test
    public void testgetBooleanDefault() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propBool"));
        // When
        Assertions.assertEquals("true", ff4jConf.getString("propBool"));
        Assertions.assertTrue(ff4jConf.getBoolean("propBool", false));
        Assertions.assertFalse(ff4jConf.getBoolean("invalid", false));
        Assertions.assertEquals(Boolean.TRUE, ff4jConf.getBoolean("propBool", Boolean.FALSE));
        Assertions.assertEquals(Boolean.FALSE, ff4jConf.getBoolean("invalid", Boolean.FALSE));
    }

    @Test
    public void testgetByteKO() {
        assertThrows(InvalidPropertyTypeException.class, () -> {
            // Given
            Assertions.assertTrue(ff4jConf.containsKey("propString"));
            // When
            ff4jConf.getByte("propString");
        });
    }
    
    @Test
    public void testgetByteOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propByte"));
        // When
        Assertions.assertEquals("123", ff4jConf.getString("propByte"));
        Assertions.assertEquals(new Byte("123"), (Byte) ff4jConf.getByte("propByte"));
        Assertions.assertEquals(new Byte("123").byteValue(), ff4jConf.getByte("propByte"));
        Assertions.assertEquals(123, ff4jConf.getByte("propByte"));
    }
    
    @Test
    public void testgetByteDefault() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propByte"));
        // When
        Assertions.assertEquals("123", ff4jConf.getString("propByte"));
        Assertions.assertEquals(new Byte("120"), (Byte) ff4jConf.getByte("invalid", new Byte("120")));
        Assertions.assertEquals(new Byte("120").byteValue(), ff4jConf.getByte("invalid", new Byte("120").byteValue()));
        Assertions.assertEquals(new Byte("123"), (Byte) ff4jConf.getByte("propByte", new Byte("120")));
        Assertions.assertEquals(new Byte("123").byteValue(), ff4jConf.getByte("propByte", new Byte("120").byteValue()));
    }
    
    @Test
    public void testgetDoubleKO() {
        assertThrows(InvalidPropertyTypeException.class, () -> {
            // Given
            Assertions.assertTrue(ff4jConf.containsKey("propString"));
            // When
            ff4jConf.getDouble("propString");
        });
    }
    
    @Test
    public void testgetDoubleOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propDouble"));
        // When
        Assertions.assertEquals(new Double(12.5), (Double) ff4jConf.getDouble("propDouble"));
        Assertions.assertEquals(new Double(12.5), new Double(ff4jConf.getDouble("propDouble")));
    }
    
    @Test
    public void testgetDoubleDefault() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propDouble"));
        Assertions.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assertions.assertEquals(new Double(12.5), (Double) ff4jConf.getDouble("propDouble", 20.5));
        Assertions.assertEquals(new Double(20.5), (Double) ff4jConf.getDouble("invalid", 20.5));
        Assertions.assertEquals(new Double(12.5), (Double) ff4jConf.getDouble("propDouble", new Double(20.5)));
        Assertions.assertEquals(new Double(20.5), (Double) ff4jConf.getDouble("invalid", new Double(20.5)));
    }
    
    @Test
    public void testgetFloatKO() {
        assertThrows(InvalidPropertyTypeException.class, () -> {
            // Given
            Assertions.assertTrue(ff4jConf.containsKey("propString"));
            // When
            ff4jConf.getFloat("propString");
        });
    }
    
    @Test
    public void testgetFloatOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propDouble"));
        // When
        Assertions.assertEquals(new Float(12.5), (Float) ff4jConf.getFloat("propDouble"));
        Assertions.assertEquals(new Float(12.5), new Float(ff4jConf.getFloat("propDouble")));
    }
    
    @Test
    public void testgetFloatDefault() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propDouble"));
        Assertions.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assertions.assertEquals(new Float(12.5), (Float) ff4jConf.getFloat("propDouble", 20.5f));
        Assertions.assertEquals(new Float(20.5), (Float) ff4jConf.getFloat("invalid", 20.5f));
        Assertions.assertEquals(new Float(12.5), (Float) ff4jConf.getFloat("propDouble", new Float(20.5)));
        Assertions.assertEquals(new Float(20.5), (Float) ff4jConf.getFloat("invalid", new Float(20.5)));
    }
    
    @Test
    public void testgetIntKO() {
        assertThrows(InvalidPropertyTypeException.class, () -> {
            // Given
            Assertions.assertTrue(ff4jConf.containsKey("propString"));
            // When
            ff4jConf.getInt("propString");
        });
    }
    
    @Test
    public void testgetIntOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propInt"));
        // When
        Assertions.assertEquals(new Integer(12), (Integer) ff4jConf.getInt("propInt"));
        Assertions.assertEquals(new Integer(12), new Integer(ff4jConf.getInt("propInt")));
    }
    
    @Test
    public void testgetIntDefault() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propInt"));
        Assertions.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assertions.assertEquals(new Integer(12), (Integer) ff4jConf.getInt("propInt", 20));
        Assertions.assertEquals(new Integer(20), (Integer) ff4jConf.getInt("invalid", 20));
        Assertions.assertEquals(new Integer(12), (Integer) ff4jConf.getInteger("propInt", new Integer(20)));
        Assertions.assertEquals(new Integer(20), (Integer) ff4jConf.getInteger("invalid", new Integer(20)));
    }
    
    @Test
    public void testgetLongKO() {
        assertThrows(InvalidPropertyTypeException.class, () -> {
            // Given
            Assertions.assertTrue(ff4jConf.containsKey("propString"));
            // When
            ff4jConf.getLong("propString");
        });
    }
    
    @Test
    public void testgetLongOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propLong"));
        // When
        Assertions.assertEquals(new Long(12), (Long) ff4jConf.getLong("propLong"));
        Assertions.assertEquals(new Long(12), new Long(ff4jConf.getLong("propLong")));
    }
    
    @Test
    public void testgetLongDefault() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propInt"));
        Assertions.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assertions.assertEquals(new Long(12), (Long) ff4jConf.getLong("propLong", 20));
        Assertions.assertEquals(new Long(20), (Long) ff4jConf.getLong("invalid", 20));
        Assertions.assertEquals(new Long(12), (Long) ff4jConf.getLong("propLong", new Long(20)));
        Assertions.assertEquals(new Long(20), (Long) ff4jConf.getLong("invalid", new Long(20)));
    }
    
    @Test
    public void testgetShortKO() {
        assertThrows(InvalidPropertyTypeException.class, () -> {
            // Given
            Assertions.assertTrue(ff4jConf.containsKey("propString"));
            // When
            ff4jConf.getShort("propString");
        });
    }
    
    @Test
    public void testgetShortOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propShort"));
        // When
        Assertions.assertEquals(new Short("12"), (Short) ff4jConf.getShort("propShort"));
        Assertions.assertEquals(new Short("12"), new Short(ff4jConf.getShort("propShort")));
    }
    
    @Test
    public void testgetShortDefault() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propInt"));
        Assertions.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assertions.assertEquals(new Short("12"), (Short) ff4jConf.getShort("propShort", new Short("20").shortValue()));
        Assertions.assertEquals(new Short("20"), (Short) ff4jConf.getShort("invalid", new Short("20").shortValue()));
        Assertions.assertEquals(new Short("12"), (Short) ff4jConf.getShort("propShort", new Short("20")));
        Assertions.assertEquals(new Short("20"), (Short) ff4jConf.getShort("invalid", new Short("20")));
    }
    
    @Test
    public void testgetBigDecimalKO() {
        assertThrows(InvalidPropertyTypeException.class, () -> {
            // Given
            Assertions.assertTrue(ff4jConf.containsKey("propString"));
            // When
            ff4jConf.getBigDecimal("propString");
        });
    }
    
    @Test
    public void testgetBigDecimalOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propDouble"));
        // When
        Assertions.assertEquals(new BigDecimal(12.5), (BigDecimal) ff4jConf.getBigDecimal("propDouble"));
    }
    
    @Test
    public void testgetBigDecimalDefault() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propDouble"));
        Assertions.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assertions.assertEquals(new BigDecimal(12.5), (BigDecimal) ff4jConf.getBigDecimal("propDouble", new BigDecimal(20.5)));
        Assertions.assertEquals(new BigDecimal(20.5), (BigDecimal) ff4jConf.getBigDecimal("invalid", new BigDecimal(20.5)));
    }
    
    @Test
    public void testgetBigIntegerKO() {
        assertThrows(InvalidPropertyTypeException.class, () -> {
            // Given
            Assertions.assertTrue(ff4jConf.containsKey("propString"));
            // When
            ff4jConf.getBigInteger("propString");
        });
    }
    
    @Test
    public void testgetBigIntegerOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propInt"));
        // When
        Assertions.assertEquals(new BigInteger("12"), (BigInteger) ff4jConf.getBigInteger("propInt"));
        Assertions.assertEquals(new BigInteger("12"), new BigInteger(ff4jConf.getString("propInt")));
    }
    
    @Test
    public void testgetBigIntegerDefault() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propInt"));
        Assertions.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assertions.assertEquals(new BigInteger("12"), (BigInteger) ff4jConf.getBigInteger("propInt", new BigInteger("20")));
        Assertions.assertEquals(new BigInteger("20"), (BigInteger) ff4jConf.getBigInteger("invalid", new BigInteger("20")));
    }
    
    @Test
    public void testgetStringDefault() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propString"));
        Assertions.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        Assertions.assertEquals("hello", ff4jConf.getString("propString", "byebye"));
        Assertions.assertEquals("byebye", ff4jConf.getString("invalid", "byebye"));
    }
    
    @Test
    public void getListInvalid() {
        assertThrows(PropertyNotFoundException.class, () -> {
            Assertions.assertFalse(ff4jConf.containsKey("toto"));
            ff4jConf.getList("toto");
        });
    }
    
    @Test
    public void getListOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propList"));
        // When
        List<Object> c = ff4jConf.getList("propList");
        // Then
        Assertions.assertTrue(Util.list("a","b","c").containsAll(c));
        Assertions.assertNotNull(ff4jConf.getList("propEmptyList"));
    }
    
    @Test
    public void getListArrayOK() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propList"));
        // When
        String[] c = ff4jConf.getStringArray("propList");
        // Then
        Assertions.assertTrue(Util.list("a","b","c").containsAll(Arrays.asList(c)));
    }
    
    @Test
    public void testgetListDefault() {
        // Given
        Assertions.assertTrue(ff4jConf.containsKey("propList"));
        Assertions.assertFalse(ff4jConf.containsKey("invalid"));
        // When
        List<Object> c1 = ff4jConf.getList("propList", Util.list("d","e","f"));
        List<Object> c2 = ff4jConf.getList("invalid",  Util.list("d","e","f"));
        // Then
        Assertions.assertTrue(Util.list("a","b","c").containsAll(c1));
        Assertions.assertTrue(Util.list("d","e","f").containsAll(c2));
    }
    
    
    @Test
    public void testNullStore() {
        assertThrows(IllegalStateException.class, () ->
            new FF4jConfiguration().ff4jStore());
    }
    
}
