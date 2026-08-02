package org.ff4j.test.store;

/*-
 * #%L
 * ff4j-test
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

import java.io.InputStream;
import java.util.Date;
import java.util.HashMap;

import org.ff4j.FF4j;
import org.ff4j.conf.XmlParser;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyDate;
import org.ff4j.property.PropertyString;


import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class InMemoryPropertyStoreTest extends PropertyStoreTestSupport {

    @Override
    protected PropertyStore initPropertyStore() {
        return  new InMemoryPropertyStore("test-ff4j-features.xml");
    }
    

    /** TDD. */
    @Test
    @Override
    public void existfilled() {
        // When-Then
        Assertions.assertTrue(testedStore.existProperty("a"));
        Assertions.assertFalse(testedStore.existProperty("koala"));
    }
    
    /** TDD. */
    @Test
    @Override
    public void valueFixed() {
        // When-Then
        Assertions.assertTrue(testedStore.existProperty("a"));
        Assertions.assertEquals("AMER", testedStore.readProperty("a").getValue());
    }
    
    public void testProperty() {
        FF4j ff4j = new FF4j(new XmlParser(), "ff4j.xml");
        ff4j.getPropertiesStore().createProperty(new PropertyDate("property_3", new Date()));
        Property<?> ap = ff4j.getPropertiesStore().readProperty("property_3");
        PropertyDate pDate = (PropertyDate) ap;
        pDate.setValue(new Date());
        ff4j.getPropertiesStore().updateProperty(pDate);
        ff4j.getPropertiesStore().deleteProperty("property_3");
        Assertions.assertFalse(testedStore.existProperty("property_3"));
    }
    
    @Test
    public void testInheritMethods() {
        InMemoryPropertyStore ip = new InMemoryPropertyStore();
        ip.importPropertiesFromXmlFile("test-ff4j-features.xml");
        Assertions.assertNotNull(ip.toJson());
        ip.isEmpty();
    }
    
    @Test
    public void testInitStores() {
        new InMemoryPropertyStore(new HashMap<String, Property<?>>());
        InputStream in =  getClass().getClassLoader().getResourceAsStream("test-ff4j-features.xml");
        new InMemoryPropertyStore(in);
    }
    
    @Test
    public void testWithInvalidFileFailed() {
        assertThrows(IllegalArgumentException.class, () -> {
            new InMemoryPropertyStore("");
        });
    }
    
    @Test
    public void testWithInvalidFileFailed2() {
        assertThrows(IllegalArgumentException.class, () -> {
            new InMemoryPropertyStore((String) null);
        });
    }
    
    @Test
    public void testInvalidXML() {
        assertThrows(IllegalArgumentException.class, () -> {
            new InMemoryPropertyStore(new HashMap<String, Property<?>>());
            InputStream in = getClass().getClassLoader().getResourceAsStream("invalid.xml");
            new InMemoryPropertyStore(in);
        });
    }

    @Test
    public void testListProperties() {
        InMemoryPropertyStore ips = new InMemoryPropertyStore();
        ips.setProperties(null);
        Assertions.assertNull(ips.listPropertyNames());
    }
    
    @Test
    public void testGetters() {
        InMemoryPropertyStore ips = new InMemoryPropertyStore();
        ips.setLocation("test-ff4j-features.xml");
        ips.setFileName("invalid.xml");
        Assertions.assertEquals("invalid.xml", ips.getFileName());
    }
    
    @Test
    public void testEmpty() {
        // Given
        InMemoryPropertyStore ips = new InMemoryPropertyStore();
        Assertions.assertTrue(ips.isEmpty());
    }
    
    @Test
    public void testEmpty2() {
        // Given
        InMemoryPropertyStore ips = new InMemoryPropertyStore();
        ips.setProperties(null);
        Assertions.assertTrue(ips.isEmpty());
    }
    
    @Test
    public void testEmpty3() {
        // Given
        InMemoryPropertyStore ips = new InMemoryPropertyStore();
        ips.createProperty(new PropertyString("P1", "v1"));
        Assertions.assertFalse(ips.isEmpty());
    }
    
    @Test
    public void testDonotImportNull() {
        assertThrows(IllegalArgumentException.class, () -> {
            InMemoryPropertyStore f = new InMemoryPropertyStore();
            f.importPropertiesFromXmlFile(null);
        });
    }
    
    @Test
    public void testDonotImportInvalid() {
        assertThrows(IllegalArgumentException.class, () -> {
            InMemoryPropertyStore f = new InMemoryPropertyStore();
            f.importPropertiesFromXmlFile("invalid.xml");
        });
    }
    
    @Test
    public void testImportTwice() {
        InMemoryPropertyStore f = new InMemoryPropertyStore();
        f.importPropertiesFromXmlFile("test-ff4j-features.xml");
        f.importPropertiesFromXmlFile("test-ff4j-features.xml");
    }
    
    

}
