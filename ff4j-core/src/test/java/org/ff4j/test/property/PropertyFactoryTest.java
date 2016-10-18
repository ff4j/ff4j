package org.ff4j.test.property;

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


import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;

import org.ff4j.property.Property;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyString;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.property.util.PropertyJsonBean;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;


public class PropertyFactoryTest {

    @Test
    public void testPropertyFactory() {
        new PropertyFactory();
        PropertyFactory.createProperty("p1", (int) 1);
        PropertyFactory.createProperty("p1", (long) 1);
        PropertyFactory.createProperty("p1", (double) 1);
        PropertyFactory.createProperty("p1", (float) 1);
        PropertyFactory.createProperty("p1", (short) 1);
        PropertyFactory.createProperty("p1", (boolean) true);
        PropertyFactory.createProperty("p1", new BigDecimal(1.1d));
        PropertyFactory.createProperty("p1", new BigInteger("1"));
        PropertyFactory.createProperty("p1", new Byte("2"));
        PropertyFactory.createProperty("p1", PropertyLogLevel.LogLevel.DEBUG);
        PropertyFactory.createProperty("p1", new PropertyString("tata"));
        PropertyFactory.createProperty("p1", new Date());
        PropertyFactory.createProperty("p1", "sample");
        PropertyFactory.createProperty("p1", Calendar.getInstance());
        
        Property<?> pList = PropertyFactory.createProperty("p1", Util.list("a","b","c"));
        Assert.assertTrue(pList.getClass().equals(PropertyString.class));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testPropertyFactory1() {
        PropertyFactory.createProperty(null, (int) 1);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testPropertyFactory3() {
        PropertyFactory.createProperty("p1", null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testPropertyFactory4() {
        PropertyFactory.createProperty("p1", "java.lang.String", "s1", "desc", null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testPropertyFactory5() {
        PropertyFactory.createProperty("p1", this);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testPropertyFactory6() {
        PropertyFactory.createProperty("p1", this);
    }
    
    @Test
    public void testPropertyFactory7() {
        PropertyFactory.createProperty("p1", PropertyString.class.getName(), "PPPP");
    }
    
    @Test
    public void testPropertyFactory8() {
        PropertyFactory.createProperty("p1", PropertyString.class.getName(), "s1", "desc", null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testPropertyFactory9() {
        PropertyFactory.createProperty("p1", PropertyString.class.getName(), "s1", "desc", Util.set("s3", "s2"));
    }
    
    @Test
    public void testCreateProperty() {
        Assert.assertNull(PropertyFactory.createProperty(null));
    }
    
    
    @Test
    public void testCreatePropertyOK() {
        PropertyJsonBean jsonBean = new PropertyJsonBean(new PropertyString("p1", "v1"));
        Assert.assertNotNull(PropertyFactory.createProperty(jsonBean));
    }
}
