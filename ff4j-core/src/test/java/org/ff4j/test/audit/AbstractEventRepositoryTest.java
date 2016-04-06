package org.ff4j.test.audit;

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

import java.util.Set;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.graph.PieSector;
import org.ff4j.audit.repository.EventRepository;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import static org.ff4j.audit.EventConstants.*;

/**
 * Superclass to test {@link EventRepository}.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public abstract class AbstractEventRepositoryTest {
    
    /** Target {@link EventRepository}. */
    protected EventRepository repo;
    
    /** Target publisher. */
    protected EventPublisher publisher;
    
    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        repo = initRepository();
        publisher = new EventPublisher(repo);
    }
    
    protected Event generateEvent(String uid, String action) {
        return new Event(SOURCE_JAVA, TARGET_FEATURE, uid, action);
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
    public void testSaveEvent() throws InterruptedException {
        // Given
        Assert.assertEquals(0, repo.getTotalEventCount());
        // When
        int limit = 50;
        for (int i = 0; i < limit; i++) {
            Thread.sleep(2);
            repo.saveEvent(generateEvent("aer", ACTION_CHECK_OK));
        }
        // Then
        Assert.assertEquals(limit, repo.getTotalEventCount());
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
    


}

