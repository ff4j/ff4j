package org.ff4j.test.strategy;

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

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.ff4j.FF4j;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.time.HourInterval;
import org.ff4j.strategy.time.OfficeHourStrategy;
import org.ff4j.test.AbstractFf4jTest;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Test different behavior or office hour
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class OfficeHourStrategyTest extends AbstractFf4jTest {
    
    /** {@inheritDoc} */
    @Override
    public FF4j initFF4j() {
        return new FF4j("test-strategy-officehour.xml");
    }
    
    @Before
    public void setUp() throws Exception {
        super.setUp();
    }
    
    private FlippingExecutionContext overrideDate(int year, int month, int day, int hour) {
        FlippingExecutionContext fec = new FlippingExecutionContext();
        Calendar c = Calendar.getInstance();
        c.set(Calendar.YEAR, year);
        c.set(Calendar.MONTH, month);
        c.set(Calendar.DAY_OF_MONTH, day);
        c.set(Calendar.HOUR_OF_DAY, hour);
        c.set(Calendar.MINUTE, 0);
        fec = new FlippingExecutionContext();
        fec.addValue(OfficeHourStrategy.OVERRIDE_DATE, c);
        return fec;
    }
    
    private void assertTrue(int year, int month, int day, int hour) {
        Assert.assertTrue(ff4j.check("first",  overrideDate(year,month,day, hour)));
    }
    
    private void assertFalse(int year, int month, int day, int hour) {
        Assert.assertFalse(ff4j.check("first",  overrideDate(year,month,day, hour)));
    }
  
    @Test
    public void assertMondays() {
        // When Monday, before 08:00
        assertFalse(2015, Calendar.MARCH, 9, 5);
        // Monday, 09:00
        assertTrue(2015, Calendar.MARCH, 9, 9);
        // Monday 13:00
        assertFalse(2015, Calendar.MARCH, 9, 13);
        // Monday, 17:00 
        assertTrue(2015, Calendar.MARCH, 9, 17);
        // Monday, 19:00 
        assertFalse(2015, Calendar.MARCH, 9, 20);
    }
    
    @Test
    public void assertSaturday() {
        // Before 08:00
        assertFalse(2015, Calendar.MARCH, 14, 5);
        // Saturday, 11:00
        assertTrue(2015, Calendar.MARCH, 14, 11);
        // Saturday, 20:00 
        assertFalse(2015, Calendar.MARCH, 14, 20);
    }
    
    @Test
    public void assertSunday() {
        assertFalse(2015, Calendar.MARCH, 15, 5);
        assertFalse(2015, Calendar.MARCH, 15, 9);
        assertFalse(2015, Calendar.MARCH, 15, 13);
        assertFalse(2015, Calendar.MARCH, 15, 17);
    }
    
    @Test
    public void assertPublicHoliday() {
        assertFalse(2015, Calendar.DECEMBER, 25, 5);
        assertFalse(2015, Calendar.DECEMBER, 25, 9);
        assertFalse(2015, Calendar.DECEMBER, 25, 13);
        assertFalse(2015, Calendar.DECEMBER, 25, 17);
    }
    
    @Test
    public void assertSpecial() {
        
        // HOLIDAY BUT OVERRIDED
        assertFalse(2015, Calendar.JANUARY, 1, 5);
        assertTrue(2015, Calendar.JANUARY, 1, 11);
        assertFalse(2015, Calendar.JANUARY, 1, 14);
        
        // WEEK BUT OVERRIDED
        assertFalse(2015, Calendar.JANUARY, 2, 5);
        assertTrue(2015, Calendar.JANUARY, 2, 11);
        assertFalse(2015, Calendar.JANUARY, 2, 14);
    }
    
     @Test
    public void testInterval() throws InterruptedException {
         new HourInterval();
         new HourInterval("08:00-12:00");
         new HourInterval(Calendar.getInstance(), Calendar.getInstance());
         Date d1 = new Date();
         Thread.sleep(2000);
         Date d2 = new Date();
         new HourInterval(d1, d2);
         new HourInterval(d2, d1);
         HourInterval h1 = new HourInterval("09:00", "12:00");
         h1.setFrom(Calendar.getInstance());h1.getFrom();
         h1.setTo(Calendar.getInstance());h1.getTo();
         h1.matches();
    }
     
     @Test
     public void testInterval2() {
         new HourInterval("18:00-12:00");
     }
     
     @Test(expected = IllegalArgumentException.class)
     public void testIntervalError() {
         new HourInterval("ksqjhdsqkjhdlh");
     }
     
     @Test(expected = IllegalArgumentException.class)
     public void testIntervalError2() {
         new HourInterval("10000", "1000");
     }
     
     @Test
     public void testOfficeHour() {
         OfficeHourStrategy ohs = new OfficeHourStrategy();
         Map < String, String > initParams = new HashMap<String, String>();
         initParams.put("monday", "08:00-12:00,13:30-18:00");
         initParams.put("tuesday", "08:00-12:00,13:30-18:00");
         initParams.put("wednesday", "08:00-12:00,13:30-18:00");
         initParams.put("thursday", "08:00-12:00,13:30-18:00");
         initParams.put("friday", "08:00-12:00,13:30-18:00");
         initParams.put("saturday", "08:00-12:00,13:30-18:00");
         initParams.put("sunday", "08:00-12:00,13:30-18:00");
         initParams.put("publicHolidays", "2015-01-01,2015-05-01,2015-12-25");
         initParams.put("specialOpenings", "[08:00-12:00]@2015-01-01;[08:00-12:00]@2015-01-02");         
         ohs.init("f1", initParams);
     }
     
     @Test
     public void testOfficeHour2() {
         OfficeHourStrategy ohs = new OfficeHourStrategy();
         Map < String, String > initParams = new HashMap<String, String>();
         initParams.put("monday", "08:00-12:00,13:30-18:00");
         initParams.put("tuesday", "08:00-12:00,13:30-18:00");
         initParams.put("wednesday", "08:00-12:00,13:30-18:00");
         initParams.put("thursday", "08:00-12:00,13:30-18:00");
         initParams.put("friday", "08:00-12:00,13:30-18:00");
         initParams.put("saturday", "08:00-12:00,13:30-18:00");
         initParams.put("sunday", "08:00-12:00,13:30-18:00");
         ohs.init("f1", initParams);
     }
     
     @Test(expected = IllegalArgumentException.class)
     public void testOfficeHour3() {
         OfficeHourStrategy ohs = new OfficeHourStrategy();
         Map < String, String > initParams = new HashMap<String, String>();
         initParams.put("publicHolidays", "20152-01-xx01,2015-05-01,2015-12-25");
         ohs.init("f1", initParams);
     }
     
     @Test(expected = IllegalArgumentException.class)
     public void testOfficeHour4() {
         OfficeHourStrategy ohs = new OfficeHourStrategy();
         Map < String, String > initParams = new HashMap<String, String>();
         initParams.put("specialOpenings", "[08:00-12:00]@2015@-01-01;[08:00-12:00]@2015-01-02"); 
         ohs.init("f1", initParams);
     }
     
     @Test(expected = IllegalArgumentException.class)
     public void testOfficeHour5() {
         OfficeHourStrategy ohs = new OfficeHourStrategy();
         Map < String, String > initParams = new HashMap<String, String>();
         initParams.put("specialOpenings", "[08:00-12:00]@2ss015-01-01;[08:00-12:00]@2015-01-02"); 
         ohs.init("f1", initParams);
     }
     
     @Test
     public void testOfficeHourEmptyExpression() {
         OfficeHourStrategy ohs = new OfficeHourStrategy();
         ohs.parseIntervalsExpression(null);
         ohs.parseIntervalsExpression("");
         ohs.matches(Calendar.getInstance(), null);
         ohs.evaluate("f1", new InMemoryFeatureStore(), null);
         ohs.evaluate("f1", new InMemoryFeatureStore(), new FlippingExecutionContext());
     }
     
     
}

