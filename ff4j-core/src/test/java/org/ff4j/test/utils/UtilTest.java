package org.ff4j.test.utils;

import java.lang.reflect.Constructor;

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

import org.ff4j.audit.EventConstants;
import org.ff4j.store.JdbcStoreConstants;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;

public class UtilTest {
    
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
	     
	     
    }

}
