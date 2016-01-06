package org.ff4j.test.property;

import java.io.InputStream;
import java.util.Date;
import java.util.HashMap;

import org.ff4j.FF4j;
import org.ff4j.property.AbstractProperty;
import org.ff4j.property.PropertyDate;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.junit.Assert;
import org.junit.Test;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

/**
 * Test for {@link InMemoryPropertyStore}.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class InMemoryPropertiesStoreTest extends AbstractPropertyStoreJunitTest {

    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        return new InMemoryPropertyStore("ff4j.xml");
    }
    
    /** TDD. */
    @Test
    public void exist_filled() {
        // When-Then
        Assert.assertTrue(testedStore.existProperty("a"));
        Assert.assertFalse(testedStore.existProperty("k"));
    }
    
    /** TDD. */
    @Test
    public void valueFixed() {
        // When-Then
        Assert.assertTrue(testedStore.existProperty("a"));
        Assert.assertEquals("AMER", testedStore.readProperty("a").getValue());
    }
    
    public void testProperty() {
        
        FF4j ff4j = new FF4j("ff4j.xml");
        
        ff4j.getPropertiesStore().createProperty(new PropertyDate("property_3", new Date()));
       
        AbstractProperty<?> ap = ff4j.getPropertiesStore().readProperty("property_3");
        PropertyDate pDate = (PropertyDate) ap;
        pDate.setValue(new Date());
        ff4j.getPropertiesStore().updateProperty(pDate);
        ff4j.getPropertiesStore().deleteProperty("property_3");
    }
    
    @Test
    public void testInheritMethods() {
        InMemoryPropertyStore ip = new InMemoryPropertyStore();
        ip.importPropertiesFromXmlFile("ff4j.xml");
        Assert.assertNotNull(ip.toJson());
        ip.isEmpty();
    }
    
    @Test
    public void testInitStores() {
        new InMemoryPropertyStore(new HashMap<String, AbstractProperty<?>>());
        InputStream in =  getClass().getClassLoader().getResourceAsStream("ff4j.xml");
        new InMemoryPropertyStore(in);
    }
    
   

}
