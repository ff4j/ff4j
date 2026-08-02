package org.ff4j.test.utils;

/*-
 * #%L
 * ff4j-core
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

import java.lang.reflect.Constructor;
import java.util.ArrayList;


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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class UtilTest {
    
    @Test
    public void testHasLength() {
        Assertions.assertFalse(Util.hasLength(null));
        Assertions.assertFalse(Util.hasLength(""));
        Assertions.assertTrue(Util.hasLength("OK"));
    }
    
    @Test
    public void testIsValidClass() {
        Assertions.assertFalse(Util.isValidClass(null));
        Assertions.assertFalse(Util.isValidClass(NullType.class));
        Assertions.assertTrue(Util.isValidClass(String.class));
    }
    
    @Test
    public void testAsserts() {
        assertThrows(IllegalArgumentException.class, () ->
            Util.assertNotNull("toto", (Object[]) null));
    }
    
    @Test
    public void testAssertsHasLength() {
        assertThrows(IllegalArgumentException.class, () ->
            Util.assertHasLength((String[]) null));
    }
    
    @Test
    public void testAssertsNotEmpty() {
        assertThrows(IllegalArgumentException.class, () ->
            Util.assertNotEmpty(null));
    }
    
    @Test
    public void testAssertsNotEmpty2() {
        assertThrows(IllegalArgumentException.class, () ->
            Util.assertNotEmpty(new ArrayList<String>()));
    }
    
    @Test
    public void testAssertsNotEmptyOK() {
        assertThrows(IllegalArgumentException.class, () -> {
            // OK
            Util.assertNotEmpty(Util.set("1", "2"));
            Util.assertParamHasNotNull(null, "sample");
        });
    }
    
    @Test
    public void testAssertsParams() {
        // OK
        Util.assertParamHasNotNull("x", "sample");
        Assertions.assertNull(Util.set((Object[]) null));
        Assertions.assertNull(Util.list((Object[])null));
        Assertions.assertNull(Util.join(null,","));
    }
    
    @Test
    public void isClassCollection() {
        Assertions.assertTrue(Util.isClassCollection(Set.class));
        Assertions.assertTrue(Util.isClassCollection(HashMap.class));
        Assertions.assertFalse(Util.isClassCollection(String.class));
        
        Assertions.assertFalse(Util.isCollection(null));
        Assertions.assertFalse(Util.isCollection("toto"));
        Assertions.assertTrue(Util.isCollection(new ArrayList<String>()));
        
        Assertions.assertTrue(Util.isEmpty(null));
        Assertions.assertTrue(Util.isEmpty(new ArrayList<String>()));
        Assertions.assertFalse(Util.isEmpty(Util.set("1")));
        
        Assertions.assertNull(Util.asCollection(null));
        Assertions.assertNotNull(Util.asCollection(new String[] {"a"}));
        Assertions.assertNotNull(Util.asCollection(Util.set("1")));
    }
    
    @Test
    public void asCollectionError() {
        assertThrows(IllegalArgumentException.class, () ->
            Util.asCollection("1"));
    }
    
    @Test
    public void testGetKeysByValue() {
        Assertions.assertNull(Util.getKeysByValue(null, "aa"));
        Map < String, String > code = new HashMap<String, String>();
        code.put("key1", "val");
        code.put("key2", "val");
        
        Set < String > keys = Util.getKeysByValue(code, "val");
        Assertions.assertNotNull(keys);
        Assertions.assertFalse(keys.isEmpty());
        Assertions.assertTrue(keys.contains("key1"));
        
        Set < String > keys2 = Util.getKeysByValue(code, "invalidval");
        Assertions.assertNotNull(keys2);
        Assertions.assertTrue(keys2.isEmpty());
    }
    
    @Test
    public void testFirstKeyByValue() {
        Assertions.assertNull(Util.getFirstKeyByValue(null, "aa"));
        Map < String, String > code = new HashMap<String, String>();
        code.put("key1", "val");
        code.put("key2", "val");
        
        Assertions.assertNull(Util.getFirstKeyByValue(code, "invalid"));
        Assertions.assertNotNull(Util.getFirstKeyByValue(code, "val"));
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
        Assertions.assertNotNull(Util.getRandomElement(g1));
        Util.getColorsGradient(9);
    }

}
