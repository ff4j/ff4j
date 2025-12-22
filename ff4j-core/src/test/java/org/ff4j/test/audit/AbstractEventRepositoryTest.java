package org.ff4j.test.audit;

/*-
 * #%L
 * ff4j-core
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

import static org.ff4j.audit.EventConstants.ACTION_CHECK_OFF;


import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.audit.EventConstants.ACTION_CREATE;
import static org.ff4j.audit.EventConstants.SOURCE_JAVA;
import static org.ff4j.audit.EventConstants.SOURCE_WEB;
import static org.ff4j.audit.EventConstants.SOURCE_WEBAPI;
import static org.ff4j.audit.EventConstants.TARGET_FEATURE;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.BarChart;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.core.Feature;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.utils.Util;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Superclass to test {@link EventRepository}.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public abstract class AbstractEventRepositoryTest {
    
    /** Feature List. */
    protected ArrayList<Feature> features;

    /** Target {@link EventRepository}. */
    protected EventRepository repo;
    
    /** Target publisher. */
    protected EventPublisher publisher;
    
    /** {@inheritDoc} */
    @BeforeEach
    public void setUp() throws Exception {
        repo      = initRepository();
        publisher = new EventPublisher(repo);
        features  = new ArrayList<Feature>(new InMemoryFeatureStore("ff4j.xml").readAll().values());
    }
   
    // Utility to generate event
    protected Event generateFeatureUsageEvent(String uid) {
        return new Event(SOURCE_JAVA, TARGET_FEATURE, uid, ACTION_CHECK_OK);
    }
    
    // Generate a random event during the period
    protected Event generateFeatureUsageEvent(String uid, long timestamp) {
        Event event = generateFeatureUsageEvent(uid);
        event.setTimestamp(timestamp);
        return event;
    }
    
    // Generate a random event during the period
    protected Event generateFeatureUsageEvent(String uid, long from, long to) {
        return generateFeatureUsageEvent(uid, from + (long) (Math.random() * (to-from)));
    }
    
    // Generate a random event during the period
    protected Event generateRandomFeatureUsageEvent(long from, long to) {
        return generateFeatureUsageEvent(Util.getRandomElement(features).getUid(), from , to);
    }
    
    // Populate repository for test
    protected void populateRepository(long from, long to, int totalEvent) throws InterruptedException {
        for (int i = 0; i < totalEvent; i++) {
            repo.saveEvent(generateRandomFeatureUsageEvent(from, to));
        }
    }
    
    /**
     * Any store test will declare its store through this callback.
     * 
     * @return working feature store
     * @throws Exception
     *             error during building feature store
     */
    protected abstract EventRepository initRepository();
    
    @Test
    public void testSaveEventUnit() throws InterruptedException {
        long start = System.currentTimeMillis();
        Assertions.assertEquals(0, repo.getFeatureUsageTotalHitCount(new EventQueryDefinition(start, System.currentTimeMillis())));
        repo.saveEvent(generateFeatureUsageEvent("f1"));
        Thread.sleep(100);
        Assertions.assertEquals(1, repo.getFeatureUsageTotalHitCount(new EventQueryDefinition(start-20, System.currentTimeMillis())));
    }
    
    @Test
    public void testSaveEventNull() {
        assertThrows(IllegalArgumentException.class, () ->
            Assertions.assertFalse(repo.saveEvent(null)));
    }
    
    @Test
    public void testSaveAuditTrail() throws InterruptedException {
        long start = System.currentTimeMillis();
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE);
        Assertions.assertTrue(repo.saveEvent(evt1));
        Thread.sleep(200);
        Assertions.assertEquals(1, repo.getAuditTrail(new EventQueryDefinition(start-10, System.currentTimeMillis())).size());
    }
    
    @Test
    public void testPieChart() throws InterruptedException {
        long start = System.currentTimeMillis();
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE);
        Assertions.assertTrue(repo.saveEvent(evt1));
        Thread.sleep(200);
        
        EventQueryDefinition eqd = new EventQueryDefinition(start-10, System.currentTimeMillis());
        Assertions.assertNotNull(repo.getFeatureUsagePieChart(eqd));
        Assertions.assertNotNull(repo.getHostPieChart(eqd));
        Assertions.assertNotNull(repo.getSourcePieChart(eqd));
        Assertions.assertNotNull(repo.getUserPieChart(eqd));
        
        Assertions.assertNotNull(repo.getHostBarChart(eqd));
        Assertions.assertNotNull(repo.getSourceBarChart(eqd));
        Assertions.assertNotNull(repo.getUserBarChart(eqd));
        
    }
    
    @Test
    public void testFeatureUsageBarCharts() throws InterruptedException {
        long start = System.currentTimeMillis();
        // Create Event
        repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE));
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
            repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE,  "f2", ACTION_CHECK_OK));
        }
        
        // Assert bar chart (2 bars with 8 and 8)
        EventQueryDefinition testQuery = new EventQueryDefinition(start-10, System.currentTimeMillis()+10);
        BarChart bChart = repo.getFeatureUsageBarChart(testQuery);
        Assertions.assertEquals(2, bChart.getChartBars().size());
        Assertions.assertEquals(new Integer(8), bChart.getChartBars().get(0).getValue());
        Assertions.assertEquals(new Integer(8), bChart.getChartBars().get(1).getValue());
        Assertions.assertNotNull(bChart.getChartBars().get(0).getColor());
        Assertions.assertNotNull(bChart.getChartBars().get(1).getColor());
    }
    
    @Test
    public void testFeatureUsageHitCount() throws InterruptedException {
        long start = System.currentTimeMillis();
        // Create Event
        repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE));
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CHECK_OK));
            repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE, "f2", EventConstants.ACTION_CHECK_OK));
        }
        Thread.sleep(100);
        
        // Assert bar chart (2 bars with 8 and 8)
        EventQueryDefinition testQuery = new EventQueryDefinition(start, System.currentTimeMillis());
        // Assert Pie Chart (2 sectors with 8 and 8)
        Map < String, MutableHitCount > mapOfHit = repo.getFeatureUsageHitCount(testQuery);
        Assertions.assertEquals(2, mapOfHit.size());
        Assertions.assertTrue(mapOfHit.containsKey("f1"));
        Assertions.assertTrue(mapOfHit.containsKey("f2"));
        Assertions.assertEquals(8, mapOfHit.get("f1").get());
    }
    
    @Test
    public void testSearchFeatureUsageEvents() throws InterruptedException {
        long start = System.currentTimeMillis();
        repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE));
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
            repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE, "f2", ACTION_CHECK_OK));
        }
        Thread.sleep(100);
        
        // Then
        EventQueryDefinition testQuery = new EventQueryDefinition(start-20, System.currentTimeMillis());
        EventSeries es = repo.searchFeatureUsageEvents(testQuery);
        Assertions.assertEquals(16, es.size());
        
        // Then
        
    }
    
    @Test
    public void testGetFeatureUsageHistory() throws InterruptedException {
        long start = System.currentTimeMillis();
        repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE));
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
            repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE, "f2", ACTION_CHECK_OK));
        }
        Thread.sleep(100);
        
        // Then
        EventQueryDefinition testQuery = new EventQueryDefinition(start-20, System.currentTimeMillis());
        TimeSeriesChart  tsc = repo.getFeatureUsageHistory(testQuery, TimeUnit.HOURS);
        Assertions.assertEquals(1, tsc.getTimeSlots().size());
    }
    
    /** TDD. */
    @Test
    public void testSourceHitCount() throws InterruptedException {
        long start = System.currentTimeMillis();
        // When
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
            repo.saveEvent(new Event(SOURCE_WEB,  TARGET_FEATURE, "f2", ACTION_CHECK_OK));
        }
        Thread.sleep(200);
        repo.saveEvent(new Event(SOURCE_WEBAPI, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
        Thread.sleep(200);
        
        // Then
        EventQueryDefinition testQuery = new EventQueryDefinition(start-20, System.currentTimeMillis());
        Map < String, MutableHitCount > mapOfHit = repo.getSourceHitCount(testQuery);
        Assertions.assertEquals(3, mapOfHit.size());
        Assertions.assertTrue(mapOfHit.containsKey(SOURCE_JAVA));
        Assertions.assertTrue(mapOfHit.containsKey(SOURCE_WEB));
        Assertions.assertEquals(1, mapOfHit.get(SOURCE_WEBAPI).get());
    }
    
    /** TDD. */
    @Test
    public void testUserHitCount() throws InterruptedException {
        long start = System.currentTimeMillis();
        // When
        for(int i = 0;i<8;i++) {
            Event e1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK);
            e1.setUser("JOHN");
            repo.saveEvent(e1);
            Thread.sleep(100);
            
            Event e2 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK);
            e2.setUser("BOB");
            repo.saveEvent(e2);
            Thread.sleep(100);
        }
        Thread.sleep(200);
        
        // Then
        EventQueryDefinition testQuery = new EventQueryDefinition(start-20, System.currentTimeMillis());
        Map < String, MutableHitCount > mapOfHit = repo.getUserHitCount(testQuery);
        Assertions.assertEquals(2, mapOfHit.size());
        Assertions.assertTrue(mapOfHit.containsKey("JOHN"));
        Assertions.assertTrue(mapOfHit.containsKey("BOB"));
        Assertions.assertEquals(8, mapOfHit.get("BOB").get());
    }
    
    /** TDD. */
    @Test
    public void testHostHitCount() throws InterruptedException {
        long start = System.currentTimeMillis();
        // When
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
        }
        Thread.sleep(200);
        
        // Then
        EventQueryDefinition testQuery = new EventQueryDefinition(start, System.currentTimeMillis());
        Map < String, MutableHitCount > mapOfHit = repo.getHostHitCount(testQuery);
        Assertions.assertEquals(1, mapOfHit.size());
        Assertions.assertEquals(1, mapOfHit.values().size());
    }
    
    /** TDD. */
    @Test
    public void testSaveCheckOff() throws InterruptedException {
        long start = System.currentTimeMillis();
        // Given
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OFF);
        // When
        Assertions.assertTrue(repo.saveEvent(evt1));
        Thread.sleep(100);
        // Then
        Assertions.assertEquals(0, repo.getFeatureUsageTotalHitCount(new EventQueryDefinition(start, System.currentTimeMillis())));
        Assertions.assertEquals(0, repo.getAuditTrail(new EventQueryDefinition(start, System.currentTimeMillis())).size());
    }
    
    /** TDD. */
    @Test
    public void testLimitEventSeries() throws InterruptedException {
        EventSeries es = new EventSeries(5);
        for(int i=0;i<10;i++) {
            Thread.sleep(10);
            es.add(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE));
        }
        Assertions.assertEquals(5, es.size());
    }
    
    /** TDD. */
    @Test
    public void testGetEventByUID() throws InterruptedException {
        // Given
        String dummyId = "1234-5678-9012-3456";
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE);
        evt1.setUuid(dummyId);
        // When
        repo.saveEvent(evt1);
        // Let the store to be updated
        Thread.sleep(100);
        // Then
        Event evt = repo.getEventByUUID(dummyId, System.currentTimeMillis());
        Assertions.assertNotNull(evt);
    }
    
    /** TDD. */
    @Test
    public void testGetEventByUID2() throws InterruptedException {
        // Given
        String dummyId = "1234-5678-9012-3456";
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE);
        evt1.setUuid(dummyId);
        // When
        repo.saveEvent(evt1);
        // Let the store to be updated
        Thread.sleep(100);
        // Then
        Event evt = repo.getEventByUUID(dummyId, null);
        Assertions.assertNotNull(evt);
    }
    
    /** TDD. */
    @Test
    public void testPurgeEvents() throws InterruptedException {
        // Given, 2 events in the repo
        long topStart = System.currentTimeMillis();
        Event evtAudit = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE);
        evtAudit.setUuid("1234-5678-9012-3456");
        Event evtFeatureUsage = new Event(SOURCE_JAVA, TARGET_FEATURE, "f2", ACTION_CHECK_OK);
        evtFeatureUsage.setUuid("1234-5678-9012-3457");
        repo.saveEvent(evtAudit);
        repo.saveEvent(evtFeatureUsage);
        Thread.sleep(100);
        Assertions.assertNotNull(repo.getEventByUUID(evtAudit.getUuid(), System.currentTimeMillis()));
        Assertions.assertNotNull(repo.getEventByUUID(evtFeatureUsage.getUuid(), System.currentTimeMillis()));
        // When
        EventQueryDefinition testQuery = new EventQueryDefinition(topStart-100, System.currentTimeMillis());
        repo.purgeFeatureUsage(testQuery);
        Assertions.assertNull(repo.getEventByUUID(evtFeatureUsage.getUuid(), System.currentTimeMillis()));
        Assertions.assertTrue(repo.searchFeatureUsageEvents(testQuery).isEmpty());
        
        // Then
        EventQueryDefinition testQuery2 = new EventQueryDefinition(topStart-100, System.currentTimeMillis());
        repo.purgeAuditTrail(testQuery2);
        Assertions.assertNull(repo.getEventByUUID(evtAudit.getUuid(), System.currentTimeMillis()));

    }

}

