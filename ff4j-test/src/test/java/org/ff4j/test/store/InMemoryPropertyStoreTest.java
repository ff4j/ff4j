package org.ff4j.test.store;

import java.io.InputStream;
import java.util.Date;
import java.util.HashMap;

import org.ff4j.FF4j;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyDate;
import org.ff4j.property.PropertyString;

/*
 * #%L
 * ff4j-test
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


import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Assert;
import org.junit.Test;

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
        Assert.assertTrue(testedStore.existProperty("a"));
        Assert.assertFalse(testedStore.existProperty("koala"));
    }
    
    /** TDD. */
    @Test
    @Override
    public void valueFixed() {
        // When-Then
        Assert.assertTrue(testedStore.existProperty("a"));
        Assert.assertEquals("AMER", testedStore.readProperty("a").getValue());
    }
    
    public void testProperty() {
        FF4j ff4j = new FF4j("ff4j.xml");
        ff4j.getPropertiesStore().createProperty(new PropertyDate("property_3", new Date()));
        Property<?> ap = ff4j.getPropertiesStore().readProperty("property_3");
        PropertyDate pDate = (PropertyDate) ap;
        pDate.setValue(new Date());
        ff4j.getPropertiesStore().updateProperty(pDate);
        ff4j.getPropertiesStore().deleteProperty("property_3");
        Assert.assertFalse(testedStore.existProperty("property_3"));
    }
    
    @Test
    public void testInheritMethods() {
        InMemoryPropertyStore ip = new InMemoryPropertyStore();
        ip.importPropertiesFromXmlFile("test-ff4j-features.xml");
        Assert.assertNotNull(ip.toJson());
        ip.isEmpty();
    }
    
    @Test
    public void testInitStores() {
        new InMemoryPropertyStore(new HashMap<String, Property<?>>());
        InputStream in =  getClass().getClassLoader().getResourceAsStream("test-ff4j-features.xml");
        new InMemoryPropertyStore(in);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testWithInvalidFileFailed() {
        new InMemoryPropertyStore("");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testWithInvalidFileFailed2() {
        new InMemoryPropertyStore((String) null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testInvalidXML() {
        new InMemoryPropertyStore(new HashMap<String, Property<?>>());
        InputStream in =  getClass().getClassLoader().getResourceAsStream("invalid.xml");
        new InMemoryPropertyStore(in);
    }

    @Test
    public void testListProperties() {
        InMemoryPropertyStore ips = new InMemoryPropertyStore();
        ips.setProperties(null);
        Assert.assertNull(ips.listPropertyNames());
    }
    
    @Test
    public void testGetters() {
        InMemoryPropertyStore ips = new InMemoryPropertyStore();
        ips.setLocation("test-ff4j-features.xml");
        ips.setFileName("invalid.xml");
        Assert.assertEquals("invalid.xml", ips.getFileName());
    }
    
    @Test
    public void testEmpty() {
        // Given
        InMemoryPropertyStore ips = new InMemoryPropertyStore();
        Assert.assertTrue(ips.isEmpty());
    }
    
    @Test
    public void testEmpty2() {
        // Given
        InMemoryPropertyStore ips = new InMemoryPropertyStore();
        ips.setProperties(null);
        Assert.assertTrue(ips.isEmpty());
    }
    
    @Test
    public void testEmpty3() {
        // Given
        InMemoryPropertyStore ips = new InMemoryPropertyStore();
        ips.createProperty(new PropertyString("P1", "v1"));
        Assert.assertFalse(ips.isEmpty());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testDonotImportNull() {
        InMemoryPropertyStore f = new InMemoryPropertyStore();
        f.importPropertiesFromXmlFile(null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testDonotImportInvalid() {
        InMemoryPropertyStore f = new InMemoryPropertyStore();
        f.importPropertiesFromXmlFile("invalid.xml");
    }
    
    @Test
    public void testImportTwice() {
        InMemoryPropertyStore f = new InMemoryPropertyStore();
        f.importPropertiesFromXmlFile("test-ff4j-features.xml");
        f.importPropertiesFromXmlFile("test-ff4j-features.xml");
    }
    
    

}
