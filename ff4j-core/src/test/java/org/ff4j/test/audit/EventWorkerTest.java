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
import static org.ff4j.audit.EventConstants.SOURCE_JAVA;
import static org.ff4j.audit.EventConstants.TARGET_FEATURE;
import static org.mockito.Mockito.doThrow;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.EventRejectedExecutionHandler;
import org.ff4j.audit.EventWorker;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.audit.repository.InMemoryEventRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

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
        Assertions.assertEquals("NAME1", ew.getName());
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
        Assertions.assertNotNull(evt);
    }

    @Test
    public void testEventRejected() {
        Assertions.assertFalse(EventRejectedExecutionHandler.isMock());
    }

    @Test
    public void configuredDiscardStrategyHandlesPublisherSaturation() throws Exception {
        CountDownLatch repositoryBlocked = new CountDownLatch(1);
        CountDownLatch releaseRepository = new CountDownLatch(1);
        EventRepository repository = mock(EventRepository.class);
        when(repository.saveEvent(org.mockito.ArgumentMatchers.any(Event.class))).thenAnswer(invocation -> {
            repositoryBlocked.countDown();
            releaseRepository.await();
            return true;
        });
        AtomicInteger rejectedTasks = new AtomicInteger();
        EventRejectedExecutionHandler handler = new EventRejectedExecutionHandler(
                EventRejectedExecutionHandler.RejectionStrategy.DISCARD) {
            @Override
            public void rejectedExecution(Runnable runnable, java.util.concurrent.ThreadPoolExecutor executor) {
                rejectedTasks.incrementAndGet();
                super.rejectedExecution(runnable, executor);
            }
        };
        EventPublisher publisher = new EventPublisher(1, 1, repository, 0L, handler);

        try {
            publisher.publish(new Event(SOURCE_JAVA, TARGET_FEATURE, "F1", ACTION_CHECK_OK));
            Assertions.assertTrue(repositoryBlocked.await(1, TimeUnit.SECONDS));
            publisher.publish(new Event(SOURCE_JAVA, TARGET_FEATURE, "F2", ACTION_CHECK_OK));

            publisher.publish(new Event(SOURCE_JAVA, TARGET_FEATURE, "F3", ACTION_CHECK_OK));

            Assertions.assertEquals(1, rejectedTasks.get());
        } finally {
            releaseRepository.countDown();
            publisher.stop();
        }
    }

}
