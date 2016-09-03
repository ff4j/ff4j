package org.ff4j.test.utils;

import java.util.ArrayList;

/*
 * #%L
 * ff4j-core
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


import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyInt;
import org.ff4j.property.PropertyLong;
import org.ff4j.property.PropertyString;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.utils.JdbcUtils;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.MappingUtil;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;


public class MappingUtilsTest {
    
    @Test
    public void testMapping() {
        Map< String, String > map = new HashMap<String, String>();
        MappingUtil.fromMap(null);
        MappingUtil.toMap(null);
        MappingUtil.toMap("");
        MappingUtil.fromMap(map);
        map.put("1", "1");
        map.put("2", "2");
        MappingUtil.fromMap(map);
    }
    
    @Test
    public void testJsonMapping() {
        JsonUtils.permissionsAsJson(null);
        JsonUtils.customPropertiesAsJson(null);
        JsonUtils.customPropertiesAsJson( new HashMap<String, Property<?>>());
        
        FeatureStore store1 = new InMemoryFeatureStore();
        FF4jCacheProxy proxy = new FF4jCacheProxy(store1, null, null);
        JsonUtils.cacheJson(proxy);
    }
    
    @Test
    public void testUtil() {
        Util.assertParamHasLength("toto", "tata");
        Set < String> ss = Util.set("one", "two");
        Assert.assertNotNull(ss);
        Util.assertTrue(true);
        Util.assertNull(null);
    }
    
    @Test(expected = FeatureAccessException.class)
    public void testIntanciateInvalidFlippingStrategy() {
       MappingUtil.instanceFlippingStrategy("f1", "com.class.invalid", new HashMap<String, String>());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAssert1() {
        Util.assertTrue(false);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAssert3() {
        Util.assertParamHasLength("", "tata");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAssert4() {
        Util.assertParamHasLength(null, "tata");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAssert2() {
        Util.assertNull("");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAssert5() {
        Util.assertNotEmpty(null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAssert6() {
        Util.assertNotEmpty(new ArrayList<String>());
    }
    
    @Test
    public void testRollbackingWithNull() {
        JdbcUtils.rollback(null);
    }
    
    @Test
    public void testJsonCustomProperties() {
        // Given
        Map < String , Property<?> > props = new HashMap<String, Property<?>>();
        props.put("f1", new PropertyString("f1","v1"));
        props.put("f2", new PropertyString("f2","v2"));
        // When
        String expression = JsonUtils.customPropertiesAsJson(props);
        // Then
        Assert.assertNotNull(expression);
    }
    
    @Test
    public void testMappingPropertyToPrimitive() {
        // Primitive -> Property
        Assert.assertEquals(PropertyInt.class.getName(), MappingUtil.mapPropertyType("int"));
        Assert.assertEquals("unknown", MappingUtil.mapPropertyType("unknown"));
        Assert.assertNull(MappingUtil.mapPropertyType(null));
        
        // Property -> Primitive
        Assert.assertEquals(PropertyInt.class.getName(), MappingUtil.mapPropertyType("int"));
    }
    
    @Test
    public void testMapSimpleClass() {
        String className = null;
        Assert.assertNull(MappingUtil.mapSimpleType(className));
        Assert.assertEquals(Property.class.getName(), MappingUtil.mapSimpleType(Property.class.getName()));
        Assert.assertEquals("long", MappingUtil.mapSimpleType(PropertyLong.class.getName()));
        
        Class<?> classType = null;
        Assert.assertNull(MappingUtil.mapSimpleType(classType));
        Assert.assertEquals(Property.class.getName(), MappingUtil.mapSimpleType(Property.class));
        Assert.assertEquals("long", MappingUtil.mapSimpleType(PropertyLong.class));
    }

}
