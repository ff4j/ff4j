package org.ff4j.test.audit;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

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

import org.ff4j.FF4j;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventBuilder;
import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.BarChart;
import org.ff4j.audit.chart.PieChart;
import org.ff4j.audit.chart.Serie;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.test.DefinedPermissionSecurityManager;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;

/**
 * Use bean Accessors to raise test coverage.
 * 
 * And I discovered some bugs...fascinating... 
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class AuditBeanTest {
    
    @Test
    public void testMutableHitCount() {
        MutableHitCount mhc = new MutableHitCount(10);
        mhc.incBy(10);
        Assert.assertEquals("20", mhc.toString());
    }
    
    @Test
    public void testEventBuider() {
        FF4j ff4j = new FF4j();
        ff4j.setAuthorizationsManager(
                new DefinedPermissionSecurityManager("a", Util.set("1", "2")));
        EventBuilder eb = new EventBuilder(ff4j);
        eb.name("FeatureX");
        Assert.assertEquals("a", eb.build().getUser());
    }
    
    @Test
    public void testAverageDuration() {
        EventSeries es = new EventSeries();
        es.add(new EventBuilder().duration(1).build());
        es.add(new EventBuilder().duration(3).build());
        Assert.assertEquals(new Double(2),  new Double(es.getAverageDuration()));
    }
    
    @Test
    public void testEvents() {
        Event evt = new Event();
        evt.put("SampleKey", "SampleValue");
        evt.getDate();
        Assert.assertNotNull(evt.toJson());
        Assert.assertEquals("SampleValue", evt.getKey("SampleKey"));
    }
    
    @Test
    public void testEventQueryDefinition() {
        EventQueryDefinition eqd = new EventQueryDefinition();
        eqd.setFrom(System.currentTimeMillis() - 100);
        eqd.setTo(System.currentTimeMillis() + 100);
        eqd.setHostFilters(Util.set("localhost"));
        eqd.setSourceFilters(Util.set("JAVA_API"));
        eqd.setActionFilters(Util.set(EventConstants.ACTION_CHECK_OK));
        eqd.setNamesFilter(Util.set("F1"));
        eqd.setFrom(new Long(eqd.getFrom()));
        eqd.setTo(new Long(eqd.getTo()));
        eqd.addFilterAction(EventConstants.ACTION_CLEAR);
        eqd.addFilterHost("MC");
        eqd.addFilterSource("WEB_CONSOLE");
        eqd.addFilterName("f2");
        Assert.assertTrue(eqd.getActionFilters().contains(EventConstants.ACTION_CHECK_OK));
        
        Assert.assertFalse(eqd.matchAction(null));
        Assert.assertTrue(eqd.matchAction(EventConstants.ACTION_CHECK_OK));
        Assert.assertFalse(eqd.matchAction(EventConstants.ACTION_CHECK_OFF));
        
        Assert.assertFalse(eqd.matchSource(null));
        Assert.assertTrue(eqd.matchSource("JAVA_API"));
        Assert.assertFalse(eqd.matchSource(EventConstants.ACTION_CHECK_OFF));
        
        Assert.assertFalse(eqd.matchHost(null));
        Assert.assertTrue(eqd.matchHost("localhost"));
        Assert.assertFalse(eqd.matchHost(EventConstants.ACTION_CHECK_OFF));
        
        Assert.assertFalse(eqd.matchName(null));
        Assert.assertTrue(eqd.matchName("f2"));
        Assert.assertFalse(eqd.matchName(EventConstants.ACTION_CHECK_OFF));
    }
    
    @Test
    public void testBarChart() {
        BarChart bc = new BarChart("Title");
        bc.setTitle("title2");
        Assert.assertEquals("title2", bc.getTitle());
        Serie<Integer> s1 = new Serie<Integer>("s1", 12);
        Serie<Integer> s2 = new Serie<Integer>("s2", 14);
        bc.getChartBars().add(s1);
        bc.getChartBars().add(s2);
        Assert.assertNotNull(bc.toString());
    }
    
    @Test
    public void testPieChart() {
        PieChart bc = new PieChart("p1");
        Serie<Integer> s1 = new Serie<Integer>("s1", 12);
        Serie<Integer> s2 = new Serie<Integer>("s2", 14);
        bc.getSectors().add(s1);
        bc.getSectors().add(s2);
        Assert.assertNotNull(bc.getSectors());
        Assert.assertNotNull(bc.toString());
    }
    
    @Test
    public void testSerie() {
        Serie<Integer> s1 = new Serie<Integer>("s1", 12);
        s1.setLabel("l2");
        s1.setColor("888888");
        s1.setValue(new Integer(13));
        Assert.assertEquals("l2", s1.getLabel());
        Assert.assertNotNull(s1.toString());
    }
    
    @Test
    public void testTimeSerieChart() {
        long top = System.currentTimeMillis();
        TimeSeriesChart t1 = new TimeSeriesChart(top - 100000, top + 100000, TimeUnit.MINUTES);
        new TimeSeriesChart(top - 100000, top + 100000, TimeUnit.HOURS);
        new TimeSeriesChart(top - 100000, top + 100000, TimeUnit.DAYS);
        new TimeSeriesChart(top - 100000, top + 100000, TimeUnit.SECONDS);
        t1.addEvent(new EventBuilder().name("f1").build());
        t1.addEvent(new EventBuilder().name("f2").build());
        
        Assert.assertNotNull(t1.getSdf());
        Assert.assertNotNull(t1.toString());
        
        TimeSeriesChart tsc1 = new TimeSeriesChart();
        tsc1.setSdf(new SimpleDateFormat("yyyy"));
        tsc1.setTimeSlots(new ArrayList<String>());
        tsc1.setSeries(new HashMap<String, Serie<Map<String,MutableHitCount>>>());
        Assert.assertNotNull(tsc1);
    }
    
    
}
