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


import org.ff4j.audit.Event;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.EventType;
import org.ff4j.audit.repository.EventRepository;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
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
        int limit = 50;
        Assert.assertEquals(0, repo.getTotalEventCount());
        for (int i = 0; i < limit; i++) {
            Thread.sleep(2);
            repo.saveEvent(new Event("aer", EventType.HIT_FLIPPED));
        }
        Assert.assertEquals(limit, repo.getTotalEventCount());
    }
    
    @Test
    @Ignore
    public void testSaveEventThroughPublisher() throws InterruptedException {
        int nb = 20;
        for (int i = 0; i < nb; i++) {
            publisher.publish(new Event("aer", EventType.HIT_FLIPPED));
            Thread.sleep(2);
        }
        Thread.sleep(50);
        Assert.assertEquals(nb, repo.getTotalEventCount());
    }
    
    @Test
    public void testCountElement() throws InterruptedException {
    }
    
   

}
