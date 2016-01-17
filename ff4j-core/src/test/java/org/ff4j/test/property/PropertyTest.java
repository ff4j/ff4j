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

import org.ff4j.property.PropertyString;
import org.ff4j.property.PropertyBigDecimal;
import org.ff4j.property.PropertyBigInteger;
import org.ff4j.property.PropertyBoolean;
import org.ff4j.property.PropertyByte;
import org.ff4j.property.PropertyCalendar;
import org.ff4j.property.PropertyDate;
import org.ff4j.property.PropertyDouble;
import org.ff4j.property.PropertyFloat;
import org.ff4j.property.PropertyInt;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLong;
import org.ff4j.property.PropertyShort;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.Util;
import org.junit.Test;
import org.springframework.util.Assert;

/**
 * Basic testing of {@link PropertyString}.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyTest {
    
    @Test
    public void tesInitPropertyString() {
        new PropertyString();
        PropertyString p1 = new PropertyString("p1");
        PropertyString p2 = new PropertyString("p2", "EAST", Util.set("EAST","WEST","SOUTH","NORTH"));
        Assert.notNull(p1.getName());
        Assert.notNull(p2.getFixedValues());
    }
    
    @Test
    public void tesInitPropertyDouble() {
        new PropertyDouble();
        PropertyDouble d1 = new PropertyDouble("d1");
        PropertyDouble d2 = new PropertyDouble("d2", 1.2);
        PropertyDouble d3 = new PropertyDouble("d3", "1.3");
        Assert.notNull(d1.getName());
        Assert.notNull(d2.getFixedValues());
        Assert.notNull(d3.getName());
    }
    
    @Test
    public void tesInitPropertyInt() {
        new PropertyInt();
        PropertyInt d1 = new PropertyInt("d1");
        PropertyInt d2 = new PropertyInt("d2", 1);
        PropertyInt d3 = new PropertyInt("d3", "2");
        Assert.notNull(d1.getName());
        Assert.notNull(d2.getFixedValues());
        Assert.notNull(d3.getName());
    }
    
    @Test
    public void tesInitPropertyBoolean() {
        new PropertyBoolean();
        PropertyBoolean d1 = new PropertyBoolean("d1");
        PropertyBoolean d2 = new PropertyBoolean("d2", true);
        PropertyBoolean d3 = new PropertyBoolean("d3", "false");
        Assert.notNull(d1.getName());
        Assert.notNull(d2.getFixedValues());
        Assert.notNull(d3.getName());
    }
    
    @Test
    public void tesInitPropertyFloat() {
        new PropertyFloat();
        PropertyFloat d1 = new PropertyFloat("d1");
        PropertyFloat d2 = new PropertyFloat("d2", 1.1F);
        PropertyFloat d3 = new PropertyFloat("d3", "1.0");
        Assert.notNull(d1.getName());
        Assert.notNull(d2.getName());
        Assert.notNull(d3.getName());
        d1.fromString("1.1");
    }
    
    @Test
    public void tesInitPropertyLong() {
        new PropertyLong();
        PropertyLong d1 = new PropertyLong("d1");
        PropertyLong d2 = new PropertyLong("d2", 1L);
        PropertyLong d3 = new PropertyLong("d3", "1");
        Assert.notNull(d1.getName());
        Assert.notNull(d2.getName());
        Assert.notNull(d3.getName());
        d1.fromString("1");
    }
    
    @Test
    public void tesInitPropertyShort() {
        new PropertyShort();
        PropertyShort d1 = new PropertyShort("d1");
        PropertyShort d2 = new PropertyShort("d2", new Short("1"));
        PropertyShort d3 = new PropertyShort("d3", "2");
        Assert.notNull(d1.getName());
        Assert.notNull(d2.getFixedValues());
        Assert.notNull(d3.getName());
        d1.fromString("1");
    }
    
    @Test
    public void tesInitPropertyBigInteger() {
        new PropertyBigInteger();
        PropertyBigInteger d1 = new PropertyBigInteger("d1");
        PropertyBigInteger d2 = new PropertyBigInteger("d2", new BigInteger("1"));
        PropertyBigInteger d3 = new PropertyBigInteger("d3", "2");
        Assert.notNull(d1.getName());
        Assert.notNull(d2.getFixedValues());
        Assert.notNull(d3.getName());
        d1.fromString("1");
    }
    
    @Test
    public void tesInitPropertyByte() {
        new PropertyByte();
        PropertyByte d1 = new PropertyByte("d1");
        PropertyByte d2 = new PropertyByte("d2", "1");
        PropertyByte d3 = new PropertyByte("d3", new Byte((byte) 100));
        Assert.notNull(d1.getName());
        Assert.notNull(d2.getName());
        Assert.notNull(d3.getName());
        d1.fromString("1");
        d1.fromString(null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void tesInitPropertyByte2() {
        PropertyByte d1 = new PropertyByte("d1");
        d1.fromString("Invalide");
    }
    
    @Test
    public void tesInitPropertyLogLevel() {
        new PropertyLogLevel();
        PropertyLogLevel d1 = new PropertyLogLevel("DEBUG");
        d1.error();d1.fatal();
        d1.warn();d1.debug();
        d1.info();d1.trace();
    }
    
    @Test
    public void tesInitPropertyBigDecimal() {
        new PropertyBigDecimal();
        PropertyBigDecimal d1 = new PropertyBigDecimal("d1");
        PropertyBigDecimal d2 = new PropertyBigDecimal("d2", new BigDecimal("1"));
        PropertyBigDecimal d3 = new PropertyBigDecimal("d3", "2");
        Assert.notNull(d1.getName());
        Assert.notNull(d2.getFixedValues());
        Assert.notNull(d3.getName());
        d1.fromString("1");
    }
    
    @Test
    public void tesInitNull() {
        PropertyBigDecimal bd = new PropertyBigDecimal();
        bd.setValue(null);
        bd.asString();
        Assert.notNull(bd.parameterizedType());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testPropertyString() {
        new PropertyString("p1", "v1", Util.set("v0", "v2"));
    }
    
    @Test
    public void tesInitPropertyDate() {
        PropertyDate d0 = new PropertyDate();
        d0.fromString("2015-01-02 13:00");
        PropertyDate d1 = new PropertyDate("d1");
        PropertyDate d2 = new PropertyDate("d2", "2015-01-02 13:00");
        Assert.notNull(d1.getName());
        Assert.notNull(d2.getName());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void tesInitPropertyDate2() {
        PropertyDate d0 = new PropertyDate();
        d0.fromString("invalid");
    }
    
    @Test
    public void tesInitPropertyCalendar() {
        PropertyCalendar d0 = new PropertyCalendar();
        PropertyCalendar d1 = new PropertyCalendar("d1");
        PropertyCalendar d2 = new PropertyCalendar("d2", "2015-01-02 13:00");
        PropertyCalendar d3 = new PropertyCalendar("d3", Calendar.getInstance());
        Assert.notNull(d1.getName());
        Assert.notNull(d2.getName());
        Assert.notNull(d3.getName());
        d0.setName("d0");
        d0.fromString("2015-01-02 13:00");
        d0.setDescription("OK");
        d0.setType(PropertyCalendar.class.getName());
        Assert.notNull(d0.toJson());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void tesInitPropertyCalendar2() {
        PropertyCalendar d0 = new PropertyCalendar();
        d0.fromString("invalid");
    }
    
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
        
    
}