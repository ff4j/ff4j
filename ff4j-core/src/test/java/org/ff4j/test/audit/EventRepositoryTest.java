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

import junit.framework.Assert;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.EventType;
import org.ff4j.audit.InMemoryEventRepository;
import org.junit.Test;

/**
 * Test for publisher and InMemory Event repository.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class EventRepositoryTest {

    @Test
    public void testAudit() throws InterruptedException {
        int nb = 500;
        EventPublisher pub = new EventPublisher();
        for (int i = 0; i < nb; i++) {
            pub.publish(new Event("aer", EventType.HIT_FLIPPED));
            Thread.sleep(2);
        }
        Assert.assertEquals(nb, pub.getRepository().getAllEvents().size());
    }

    @Test
    public void testAuditWithLimit() throws InterruptedException {
        int nb = 500;
        int limit = 25;
        EventPublisher pub = new EventPublisher();
        pub.setRepository(new InMemoryEventRepository(limit));
        for (int i = 0; i < nb; i++) {
            pub.publish(new Event("aer", EventType.HIT_FLIPPED));
            Thread.sleep(2);
        }
        Assert.assertEquals(limit, pub.getRepository().getAllEvents().size());
    }

    @Test
    public void testCurve() throws InterruptedException {
        // Events to generate
        int nbEvent = 100;

        EventPublisher pub = new EventPublisher();
        for (int i = 0; i < nbEvent; i++) {
            pub.publish(new Event("aer", EventType.HIT_FLIPPED));
            Thread.sleep(2);
        }
        Assert.assertEquals(nbEvent, pub.getRepository().getAllEvents().size());
        long now = System.currentTimeMillis();
        pub.getRepository().getHitCurve("aer", 10, now - 3 * nbEvent, now);
    }
}
