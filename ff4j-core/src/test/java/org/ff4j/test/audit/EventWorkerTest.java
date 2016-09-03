package org.ff4j.test.audit;

import static org.ff4j.audit.EventConstants.ACTION_CHECK_OFF;
import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.audit.EventConstants.SOURCE_JAVA;
import static org.ff4j.audit.EventConstants.TARGET_FEATURE;
import static org.mockito.Mockito.doThrow;

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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.EventRejectedExecutionHandler;
import org.ff4j.audit.EventWorker;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.audit.repository.InMemoryEventRepository;
import org.junit.Assert;
import org.junit.Test;

public class EventWorkerTest {
    
    @Test
    public void testEventWorker() {
        // Given
        EventRepository er = new InMemoryEventRepository();
        Event evt = new Event(SOURCE_JAVA, TARGET_FEATURE, "F1", ACTION_CHECK_OFF);
        EventWorker ew = new EventWorker(evt, er);
        // When
        ew.setName("NAME1");
        // Then
        Assert.assertEquals("NAME1", ew.getName());
    }
    
    @Test
    public void testEventWorkerCall() throws Exception {
        // Given
        EventRepository er = mock(EventRepository.class);
        Event evt = new Event(SOURCE_JAVA, TARGET_FEATURE, "F1", ACTION_CHECK_OK);
        when(er.saveEvent(evt)).thenReturn(false);
        EventWorker ew = new EventWorker(evt, er);
        // When
        ew.call();
    }
    
    @Test
    public void testErrorOnSubmitEventPublisher() {
        // Given
        EventRepository er = mock(EventRepository.class);
        Event evt = new Event(SOURCE_JAVA, TARGET_FEATURE, "F1", ACTION_CHECK_OFF);
        doThrow(new RuntimeException("Erreur")).when(er).saveEvent(evt);
        EventPublisher evtPublisher = new EventPublisher(er);
        evtPublisher.publish(evt);
        Assert.assertNotNull(evt);
    }
    
    @Test
    public void testEventRejected() {
        Assert.assertFalse(EventRejectedExecutionHandler.isMock());
    }

}