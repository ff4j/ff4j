package org.ff4j.utils.json;

/*-
 * #%L
 * ff4j-utils-json
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


import org.ff4j.property.Property;
import org.ff4j.utils.Util;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class PropertyJsonParserTest {
    
    
    @Test
    public void testInit() throws Exception {
        Assertions.assertNotNull(Util.instanciatePrivate(PropertyJsonParser.class));
    }
    
    @Test
    public void testParsePropertyEmpty() {
        Assertions.assertNull(PropertyJsonParser.parseProperty(""));
        Assertions.assertNull(PropertyJsonParser.parseProperty(null));
    }
    
    @Test
    public void testParsePropertyError() {
        assertThrows(IllegalArgumentException.class, () ->
            Assertions.assertNull(PropertyJsonParser.parseProperty("something:invalid")));
    }
    
    @Test
    public void testFull() {
        String pExp = "{\"name\":\"p1\",\"description\":null,\"type\":\"org.ff4j.property.PropertyString\",\"value\":\"v1\",\"fixedValues\":null}";
        Property<?> p = PropertyJsonParser.parseProperty(pExp);
        Assertions.assertNotNull(p);
        Assertions.assertNotNull(p.getType());
        Assertions.assertEquals("v1", p.getValue());
    }
    
    

}
