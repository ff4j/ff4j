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
import org.ff4j.audit.EventType;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.graph.PieSector;
import org.ff4j.audit.repository.EventRepository;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Superclass to test {@link EventRepository}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
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
            repo.saveEvent(new Event("aer", EventType.FEATURE_CHECK_ON));
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
            Event evt = new Event("aer", EventType.FEATURE_CHECK_ON);
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
        repo.saveEvent(new Event("F1", EventType.FEATURE_CHECK_ON));
        repo.saveEvent(new Event("F2", EventType.FEATURE_CHECK_ON));
        repo.saveEvent(new Event("F3", EventType.FEATURE_CHECK_ON));
        
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
            repo.saveEvent(new Event("evt1", EventType.FEATURE_CHECK_ON));
            repo.saveEvent(new Event("evt2", EventType.FEATURE_CHECK_ON));
            Thread.sleep(2);
        }
        long endTime = System.currentTimeMillis();
        long startTime = endTime - (5 * nbEvent);
        BarChart bc = repo.getHitsBarChart(startTime, endTime, nbSlot);
        
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
            repo.saveEvent(new Event("evt1",  EventType.FEATURE_CHECK_ON));
            if (i%2 == 0)
                repo.saveEvent(new Event("evt1",  EventType.FEATURE_CHECK_OFF));
            if (i%5 == 0)
                repo.saveEvent(new Event("evt1",  EventType.DISABLE_FEATURE));
            Thread.sleep(2);
        }
        long endTime = System.currentTimeMillis();
        long startTime = endTime - (4 * nbEvent);
        PieChart pc = repo.getFeatureHitsPie("evt1", startTime, endTime);
        
        // Then
        Assert.assertEquals(34, repo.getTotalEventCount());
        Assert.assertEquals(3, pc.getSectors().size());

        PieSector checkon = pc.getSectors().get(1);
        Assert.assertNotNull(checkon);
        Assert.assertEquals(EventType.FEATURE_CHECK_ON.toString(), checkon.getLabel());
    }
    

}

