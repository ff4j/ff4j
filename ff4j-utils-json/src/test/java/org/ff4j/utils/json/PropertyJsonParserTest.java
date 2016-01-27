package org.ff4j.utils.json;

/*
 * #%L
 * ff4j-utils-json
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
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;

public class PropertyJsonParserTest {
    
    
    @Test
    public void testInit() throws Exception {
        Assert.assertNotNull(Util.instanciatePrivate(PropertyJsonParser.class));
    }
    
    @Test
    public void testParsePropertyEmpty() {
        Assert.assertNull(PropertyJsonParser.parseProperty(""));
        Assert.assertNull(PropertyJsonParser.parseProperty(null));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsePropertyError() {
        Assert.assertNull(PropertyJsonParser.parseProperty("something:invalid"));
    }
    
    @Test
    public void testFull() {
        String pExp = "{\"name\":\"p1\",\"description\":null,\"type\":\"org.ff4j.property.PropertyString\",\"value\":\"v1\",\"fixedValues\":null}";
        Property<?> p = PropertyJsonParser.parseProperty(pExp);
        Assert.assertNotNull(p);
        Assert.assertNotNull(p.getType());
        Assert.assertEquals("v1", p.getValue());
    }
    
    

}
