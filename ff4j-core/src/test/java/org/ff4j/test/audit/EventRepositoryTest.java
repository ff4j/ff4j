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

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.EventType;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.repository.InMemoryEventRepository;
import org.junit.Assert;
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
        Assert.assertEquals(nb, pub.getRepository().getTotalEventCount());
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
        Assert.assertEquals(limit, pub.getRepository().getTotalEventCount());
    }

    @Test
    public void testBarChart() throws InterruptedException {
        // Given
        int nbEvent = 100;
        int nbSlot  = 10;
        
        EventPublisher pub = new EventPublisher();
        // When
        for (int i = 0; i < nbEvent; i++) {
            pub.publish(new Event("evt1", EventType.HIT_FLIPPED));
            pub.publish(new Event("evt2", EventType.HIT_FLIPPED));
            Thread.sleep(2);
        }
        long endTime = System.currentTimeMillis();
        long startTime = endTime - (3 * nbEvent);
        BarChart bc = pub.getRepository().getHitsBarChart(startTime, endTime, nbSlot);
        // Then
        Assert.assertEquals(2*nbEvent, pub.getRepository().getTotalEventCount());
        Assert.assertNotNull(bc);
        Assert.assertEquals(2, bc.getSeries().size());
        Assert.assertEquals(nbSlot, bc.getLabels().size());
        Assert.assertNotNull(bc.getSeries().get("evt1"));
        Assert.assertNotNull(bc.getSeries().get("evt2"));
        Assert.assertEquals(nbSlot, bc.getSeries().get("evt1").getValues().size());
        Assert.assertEquals(nbSlot, bc.getSeries().get("evt2").getValues().size());
        for (int i = 0; i < nbSlot; i++) {
            System.out.println(bc.getLabels().get(i) + 
                    ", evt1<" +  bc.getSeries().get("evt1").getValues().get(i).intValue() + 
                    ">, evt2<" +  bc.getSeries().get("evt2").getValues().get(i).intValue() + ">");
            
        }
        System.out.println(bc.toJson());
    }
    
    @Test
    public void testCalendar() {
        Calendar c2 = Calendar.getInstance();
        c2.setTime(new Date(System.currentTimeMillis() + 1000 * 3600 * 24));
        c2.set(Calendar.HOUR_OF_DAY, 0);
        c2.set(Calendar.MINUTE, 0);
        c2.set(Calendar.SECOND, 0);
        
        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm YYYY-MM-dd");
        System.out.println(sdf.format(c2.getTime()));
    }
}
