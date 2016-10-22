package org.ff4j.test.utils;

import java.lang.reflect.Constructor;
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
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.lang.model.type.NullType;

import org.ff4j.audit.EventConstants;
import org.ff4j.store.JdbcStoreConstants;
import org.ff4j.utils.TimeUtils;
import org.ff4j.utils.Util;
import org.ff4j.web.FF4jWebConstants;
import org.junit.Assert;
import org.junit.Test;

public class UtilTest {
    
    @Test
    public void testHasLength() {
        Assert.assertFalse(Util.hasLength(null));
        Assert.assertFalse(Util.hasLength(""));
        Assert.assertTrue(Util.hasLength("OK"));
    }
    
    @Test
    public void testIsValidClass() {
        Assert.assertFalse(Util.isValidClass(null));
        Assert.assertFalse(Util.isValidClass(NullType.class));
        Assert.assertTrue(Util.isValidClass(String.class));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAsserts() {
        Util.assertNotNull("toto", (Object[]) null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAssertsHasLength() {
        Util.assertHasLength((String[]) null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAssertsNotEmpty() {
        Util.assertNotEmpty(null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAssertsNotEmpty2() {
        Util.assertNotEmpty(new ArrayList<String>());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAssertsNotEmptyOK() {
        // OK
        Util.assertNotEmpty(Util.set("1","2"));
        Util.assertParamHasNotNull(null, "sample");
    }
    
    @Test
    public void testAssertsParams() {
        // OK
        Util.assertParamHasNotNull("x", "sample");
        Assert.assertNull(Util.set((Object[]) null));
        Assert.assertNull(Util.list((Object[])null));
        Assert.assertNull(Util.join(null,","));
    }
    
    @Test
    public void isClassCollection() {
        Assert.assertTrue(Util.isClassCollection(Set.class));
        Assert.assertTrue(Util.isClassCollection(HashMap.class));
        Assert.assertFalse(Util.isClassCollection(String.class));
        
        Assert.assertFalse(Util.isCollection(null));
        Assert.assertFalse(Util.isCollection("toto"));
        Assert.assertTrue(Util.isCollection(new ArrayList<String>()));
        
        Assert.assertTrue(Util.isEmpty(null));
        Assert.assertTrue(Util.isEmpty(new ArrayList<String>()));
        Assert.assertFalse(Util.isEmpty(Util.set("1")));
        
        Assert.assertNull(Util.asCollection(null));
        Assert.assertNotNull(Util.asCollection(new String[] {"a"}));
        Assert.assertNotNull(Util.asCollection(Util.set("1")));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void asCollectionError() {
        Util.asCollection("1");
    }
    
    @Test
    public void testGetKeysByValue() {
        Assert.assertNull(Util.getKeysByValue(null, "aa"));
        Map < String, String > code = new HashMap<String, String>();
        code.put("key1", "val");
        code.put("key2", "val");
        
        Set < String > keys = Util.getKeysByValue(code, "val");
        Assert.assertNotNull(keys);
        Assert.assertFalse(keys.isEmpty());
        Assert.assertTrue(keys.contains("key1"));
        
        Set < String > keys2 = Util.getKeysByValue(code, "invalidval");
        Assert.assertNotNull(keys2);
        Assert.assertTrue(keys2.isEmpty());
    }
    
    @Test
    public void testFirstKeyByValue() {
        Assert.assertNull(Util.getFirstKeyByValue(null, "aa"));
        Map < String, String > code = new HashMap<String, String>();
        code.put("key1", "val");
        code.put("key2", "val");
        
        Assert.assertNull(Util.getFirstKeyByValue(code, "invalid"));
        Assert.assertNotNull(Util.getFirstKeyByValue(code, "val"));
    }
    
    @Test
    public void testConstants() throws Exception {
         Constructor<JdbcStoreConstants> ce = JdbcStoreConstants.class.getDeclaredConstructor();
         ce.setAccessible(true);
         ce.newInstance();
         
         Constructor<EventConstants> de = EventConstants.class.getDeclaredConstructor();
         de.setAccessible(true);
         de.newInstance();
         
         Constructor<FF4jWebConstants> ee = FF4jWebConstants.class.getDeclaredConstructor();
         ee.setAccessible(true);
         ee.newInstance();
         
         Constructor<TimeUtils> ff = TimeUtils.class.getDeclaredConstructor();
         ff.setAccessible(true);
         ff.newInstance();
    }
    
    @Test
    public void testGradient() {
        List < String > g1 = Util.generateHSVGradient("ee1100", "442299", 9);
        Util.generateHSVGradient("442299", "ee1100", 9);
        Util.generateRGBGradient("ee1100", "442299", 9);
        Util.generateRGBGradient("442299", "ee1100", 9);
        Assert.assertNotNull(Util.getRandomElement(g1));
        Util.getColorsGradient(9);
    }

}