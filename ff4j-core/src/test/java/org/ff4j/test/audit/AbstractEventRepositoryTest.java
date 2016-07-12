package org.ff4j.test.audit;

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


import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.audit.EventConstants.SOURCE_JAVA;
import static org.ff4j.audit.EventConstants.TARGET_FEATURE;

import java.util.ArrayList;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.core.Feature;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

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
    @Before
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
    @Ignore
    public void testSaveEventUnit() {
        long start = System.currentTimeMillis();
        Assert.assertEquals(0, repo.getFeatureUsageTotalHitCount(new EventQueryDefinition(start, System.currentTimeMillis())));
        repo.saveEvent(generateFeatureUsageEvent("f1"));
        Assert.assertEquals(1, repo.getFeatureUsageTotalHitCount(new EventQueryDefinition(start, System.currentTimeMillis())));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testSaveEventNull() {
        Assert.assertFalse(repo.saveEvent(null));
    }
    
    @Test
    @Ignore
    public void testSaveAuditTrail() throws InterruptedException {
        long start = System.currentTimeMillis();
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE);
        Assert.assertTrue(repo.saveEvent(evt1));
        Thread.sleep(100);
        Assert.assertEquals(1, repo.getAuditTrail(new EventQueryDefinition(start, System.currentTimeMillis())).size());
    }
    
    @Test
    public void testSaveCheckOff() throws InterruptedException {
        long start = System.currentTimeMillis();
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CHECK_OFF);
        Assert.assertTrue(repo.saveEvent(evt1));
        Thread.sleep(100);
        Assert.assertEquals(0, repo.getFeatureUsageTotalHitCount(new EventQueryDefinition(start, System.currentTimeMillis())));
        Assert.assertEquals(0, repo.getAuditTrail(new EventQueryDefinition(start, System.currentTimeMillis())).size());
    }
    
    @Test
    public void testLimitEventSeries() throws InterruptedException {
        EventSeries es = new EventSeries(5);
        for(int i=0;i<10;i++) {
            Thread.sleep(10);
            es.add(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE));
        }
        Assert.assertEquals(5, es.size());
    }
    
    public void testgetFeatureUsageHitCount() {
        long start = System.currentTimeMillis();
        Assert.assertEquals(0, repo.getFeatureUsageTotalHitCount(new EventQueryDefinition(start, System.currentTimeMillis())));
        generateFeatureUsageEvent("x", start, System.currentTimeMillis());
        Assert.assertEquals(1, repo.getFeatureUsageTotalHitCount(new EventQueryDefinition(start, System.currentTimeMillis())));
    }
    
    /*
    @Test
    public void testSaveEvent() throws InterruptedException {
        // Given
        long xNow = System.currentTimeMillis();
        long x12HoursAgo = System.currentTimeMillis() - 1000 * 3600 * 12;
        Assert.assertEquals(0, repo.getFeatureUsageTotalHitCount(x12HoursAgo, xNow));
        // When
        int totalEvent = 50;
        for (int i = 0; i < totalEvent; i++) {
            Thread.sleep(2);
            Event rdmEvent = generateRandomEvent(12);
            repo.saveEvent(rdmEvent);
        }
        // Then
        Thread.sleep(200);
        Assert.assertEquals(totalEvent, 
                repo.getFeatureUsageTotalHitCount(x12HoursAgo, System.currentTimeMillis()));
       
        // When
        BarChart bc = (repo.getFeatureUsageBarChart(x12HoursAgo, System.currentTimeMillis()));
        // Then, check that the sum is the total
        Assert.assertEquals(features.size(), bc.getChartBars().size());
        int totalHit = 0;
        for (Bar bar : bc.getChartBars()) {
            totalHit += bar.getValue();
        }
        Assert.assertEquals(totalEvent, totalHit);
        
    }
    
    /*
    @Test
    public void testGetTotalCount() throws InterruptedException {
        // Given
        long xNow = System.currentTimeMillis();
        long x12HoursAgo = System.currentTimeMillis() - 1000 * 3600 * 12;
        Assert.assertEquals(0, repo.getFeatureUsageTotalHitCount(x12HoursAgo, xNow));
        // When
        int totalEvent = 50;
        for (int i = 0; i < totalEvent; i++) {
            Thread.sleep(2);
            Event rdmEvent = generateRandomEvent(12);
            repo.saveEvent(rdmEvent);
        }
        // Then
        Thread.sleep(200);
        Assert.assertEquals(totalEvent, 
                repo.getFeatureUsageTotalHitCount(x12HoursAgo, System.currentTimeMillis()));
    }
    
   
    
    @Test
    public void testSaveEventThroughPublisher() throws InterruptedException {
        
        // Given
        int nb = 20;
        
        // When
        for (int i = 0; i < nb; i++) {
            Event evt = new Event(SOURCE_JAVA, TARGET_FEATURE, "aer", ACTION_CHECK_OK);
            publisher.publish(evt);
            Thread.sleep(2);
        }
        Thread.sleep(50);
        
        // Then
        Assert.assertEquals(nb, repo.getTotalEventCount());
    }
    
    @Test
    public void testgetFeatureNames() {
        
        // Given
        Assert.assertEquals(0, repo.getTotalEventCount());
        Assert.assertTrue(repo.getFeatureNames().isEmpty());
        
        // When
        repo.saveEvent(generateEvent("F1", ACTION_CHECK_OK));
        repo.saveEvent(generateEvent("F2", ACTION_CHECK_OK));
        repo.saveEvent(generateEvent("F3", ACTION_CHECK_OK));
        
        // Then
        Set < String > features =  repo.getFeatureNames();
        Assert.assertEquals(3, repo.getTotalEventCount());
        Assert.assertNotNull(features);
        Assert.assertEquals(3, features.size());
        Assert.assertTrue(features.contains("F1"));
        Assert.assertFalse(features.contains("F4"));
    }
    
    @Test
    public void testgetHitsBarChart() throws InterruptedException {
        
        // Given
        int nbEvent = 50;
        int nbSlot  = 10;
        
        // When
        for (int i = 0; i < nbEvent; i++) {
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "evt1", ACTION_CHECK_OK));
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "evt2", ACTION_CHECK_OK));
            Thread.sleep(2);
        }
        long endTime = System.currentTimeMillis();
        long startTime = endTime - (5 * nbEvent);
        BarChart bc = repo.getFeaturesUsageOverTime(startTime, endTime, nbSlot);
        
        // Then
       
        // Total events are all EVT1 + all evt2
        Assert.assertEquals(2*nbEvent, repo.getTotalEventCount());
        // CHeck that service return a barchart
        Assert.assertNotNull(bc);
        // There is only 2 distinct features (evt1 & evt2)
        Assert.assertEquals(2, bc.getSeries().size());
        Assert.assertNotNull(bc.getSeries().get("evt1"));
        Assert.assertNotNull(bc.getSeries().get("evt2"));
        // There is 10 bar in the chart as expected
        Assert.assertEquals(nbSlot, bc.getLabels().size());
        // For a serie, I have the same amount of value than of slots
        Assert.assertEquals(nbSlot, bc.getSeries().get("evt1").getValues().size());
        Assert.assertEquals(nbSlot, bc.getSeries().get("evt2").getValues().size());
    }
    
    @Test
    public void testFeatureHits() throws InterruptedException {
        // Given
        int nbEvent = 20;
        
        // When
        for (int i = 0; i < nbEvent; i++) {
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "evt1", ACTION_CHECK_OK));
            if (i%2 == 0)
                repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "evt1", ACTION_CHECK_OK));
            if (i%5 == 0)
                repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "evt1", ACTION_UPDATE));
            Thread.sleep(2);
        }
        long endTime = System.currentTimeMillis();
        long startTime = endTime - (4 * nbEvent);
        PieChart pc = repo.featureDistributionPie("evt1", startTime, endTime);
        
        // Then
        Assert.assertEquals(34, repo.getTotalEventCount());
     
        if ( pc.getSectors() != null && pc.getSectors().size() > 0) {
            PieSector checkon = pc.getSectors().get(1);
            Assert.assertNotNull(checkon);
        }
    }
    
    @Test
    public void testDistributionALL() throws InterruptedException {
        int loopCount = 20;
        for(int i=0;i<loopCount;i++) {
            repo.saveEvent(generateEvent("f1", ACTION_CHECK_OK));
            repo.saveEvent(generateEvent("f2", ACTION_CHECK_OK));
            repo.saveEvent(generateEvent("f3", ACTION_CHECK_OK));
            Thread.sleep(100);
        }
        
        PieChart pie1 = repo.featuresListDistributionPie((System.currentTimeMillis() - 5000), 
                                                  (System.currentTimeMillis()));
        Assert.assertEquals(3, pie1.getSectors().size());
        Assert.assertEquals(loopCount, (int) pie1.getSectors().get(1).getValue());
    }
    
    @Test
    public void testDistribution() throws InterruptedException {
        repo.saveEvent(generateEvent("f1", ACTION_CHECK_OK));
        repo.saveEvent(generateEvent("f1", ACTION_UPDATE));
        repo.saveEvent(generateEvent("f1", ACTION_DELETE));
        
        PieChart pie2 = repo.featureDistributionPie("f1", 
                (System.currentTimeMillis() - 10000), (System.currentTimeMillis() + 10000));
        Assert.assertEquals(3, pie2.getSectors().size());
    }
    */


}

