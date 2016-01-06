package org.ff4j.test.audit;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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
import org.ff4j.audit.EventType;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.audit.repository.InMemoryEventRepository;
import org.junit.Assert;
import org.junit.Test;

/**
 * Test for publisher and InMemory Event repository.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class InMemoryEventRepositoryTest extends AbstractEventRepositoryTest {

    private int limit = 60;
    
    /** {@inheritDoc} */
    @Override
    protected EventRepository initRepository() {
        return new InMemoryEventRepository(limit);
    }

    @Test
    public void testNotExceedLimit() throws InterruptedException {
        for (int i = 0; i < (2 * limit); i++) {
            publisher.publish(new Event("aer", EventType.FEATURE_CHECK_ON));
            Thread.sleep(2);
        }
        Assert.assertEquals(limit, repo.getTotalEventCount());
        for (int i = 0; i < (2 * limit); i++) {
            publisher.publish(new Event("aer", EventType.FEATURE_CHECK_OFF));
            Thread.sleep(2);
        }
        publisher.publish(new Event("aer", EventType.ENABLE_FEATURE));
        repo.toString();
        
        repo.getHitsPieChart((System.currentTimeMillis() - 10000), (System.currentTimeMillis() + 10000));
        
    }
    
    @Test
    public void testNotExceedLimit2() throws InterruptedException {
        InMemoryEventRepository repo1 = new InMemoryEventRepository(limit);
        repo1.toString();
    }
    
    @Test
    public void testTo() throws InterruptedException {
        for (int i = 0; i < (2 * limit); i++) {
            publisher.publish(new Event("aer", EventType.FEATURE_CHECK_ON));
            Thread.sleep(2);
        }
        Assert.assertEquals(limit, repo.getTotalEventCount());
    }
    
    
}
