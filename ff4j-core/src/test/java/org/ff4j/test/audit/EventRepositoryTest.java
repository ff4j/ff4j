package org.ff4j.test.audit;

import junit.framework.Assert;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.EventRepositoryVM;
import org.ff4j.audit.EventType;
import org.junit.Test;

public class EventRepositoryTest {

    @Test
    public void testAudit() throws InterruptedException {
        int nb = 1000;
        EventPublisher pub = new EventPublisher();
        for (int i = 0; i < nb; i++) {
            pub.publish(new Event("aer", EventType.HIT_FLIPPED));
            Thread.sleep(2);
        }
        Assert.assertEquals(nb, pub.getRepository().getAllEvents().size());
    }

    @Test
    public void testAuditWithLimit() throws InterruptedException {
        int nb = 1000;
        int limit = 25;
        EventPublisher pub = new EventPublisher();
        pub.setRepository(new EventRepositoryVM(limit));
        for (int i = 0; i < nb; i++) {
            pub.publish(new Event("aer", EventType.HIT_FLIPPED));
            Thread.sleep(2);
        }
        Assert.assertEquals(limit, pub.getRepository().getAllEvents().size());

    }
}
