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

import static org.ff4j.audit.EventRejectedExecutionHandler.RejectionStrategy.DISCARD;
import static org.ff4j.audit.EventRejectedExecutionHandler.RejectionStrategy.RETRY_BOUNDED;
import static org.ff4j.audit.EventRejectedExecutionHandler.RejectionStrategy.RETRY_UNBOUNDED;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.FutureTask;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.EventRejectedExecutionHandler;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class EventRejectedExecutionHandlerTest {

    @Test
    public void discardCancelsRejectedTask() {
        FutureTask<Boolean> task = new FutureTask<Boolean>(() -> true);
        EventRejectedExecutionHandler handler = new EventRejectedExecutionHandler(DISCARD);

        handler.rejectedExecution(task, null);

        Assertions.assertTrue(task.isCancelled());
    }

    @Test
    public void boundedRetryCancelsTaskAfterMaximumRetries() {
        ThreadPoolExecutor executor = newExecutor();
        executor.getQueue().offer(new FutureTask<Boolean>(() -> true));
        CountingHandler handler = new CountingHandler(RETRY_BOUNDED, 3);
        FutureTask<Boolean> rejectedTask = new FutureTask<Boolean>(() -> true);

        handler.rejectedExecution(rejectedTask, executor);

        Assertions.assertEquals(3, handler.getRetryCount());
        Assertions.assertTrue(rejectedTask.isCancelled());
        executor.shutdownNow();
    }

    @Test
    public void unboundedRetryQueuesTaskWhenCapacityBecomesAvailable() {
        ThreadPoolExecutor executor = newExecutor();
        executor.getQueue().offer(new FutureTask<Boolean>(() -> true));
        QueueReleasingHandler handler = new QueueReleasingHandler();
        FutureTask<Boolean> rejectedTask = new FutureTask<Boolean>(() -> true);

        handler.rejectedExecution(rejectedTask, executor);

        Assertions.assertEquals(2, handler.getRetryCount());
        Assertions.assertFalse(rejectedTask.isCancelled());
        Assertions.assertTrue(executor.getQueue().contains(rejectedTask));
        executor.shutdownNow();
    }

    @Test
    public void constructorRejectsInvalidRetrySettings() {
        Assertions.assertThrows(IllegalArgumentException.class,
                () -> new EventRejectedExecutionHandler(null, 1, 1L));
        Assertions.assertThrows(IllegalArgumentException.class,
                () -> new EventRejectedExecutionHandler(RETRY_BOUNDED, -1, 1L));
        Assertions.assertThrows(IllegalArgumentException.class,
                () -> new EventRejectedExecutionHandler(RETRY_BOUNDED, 1, -1L));
    }

    @Test
    public void interruptedRetryCancelsTaskAndRestoresInterrupt() {
        ThreadPoolExecutor executor = newExecutor();
        executor.getQueue().offer(new FutureTask<Boolean>(() -> true));
        EventRejectedExecutionHandler handler = new EventRejectedExecutionHandler(RETRY_UNBOUNDED, 0, 1L);
        FutureTask<Boolean> rejectedTask = new FutureTask<Boolean>(() -> true);
        Thread.currentThread().interrupt();

        try {
            handler.rejectedExecution(rejectedTask, executor);

            Assertions.assertTrue(Thread.currentThread().isInterrupted());
            Assertions.assertTrue(rejectedTask.isCancelled());
        } finally {
            Thread.interrupted();
            executor.shutdownNow();
        }
    }

    @Test
    public void stoppedExecutorCancelsTaskWithoutRetrying() {
        ThreadPoolExecutor executor = newExecutor();
        executor.shutdownNow();
        CountingHandler handler = new CountingHandler(RETRY_UNBOUNDED, 0);
        FutureTask<Boolean> rejectedTask = new FutureTask<Boolean>(() -> true);

        handler.rejectedExecution(rejectedTask, executor);

        Assertions.assertEquals(1, handler.getRetryCount());
        Assertions.assertTrue(rejectedTask.isCancelled());
    }

    private ThreadPoolExecutor newExecutor() {
        return new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue<Runnable>(1));
    }

    private static class CountingHandler extends EventRejectedExecutionHandler {

        private int retryCount;

        CountingHandler(RejectionStrategy strategy, int maxRetries) {
            super(strategy, maxRetries, 0L);
        }

        @Override
        public void waitInMillis(long milliseconds) {
            retryCount++;
        }

        int getRetryCount() {
            return retryCount;
        }
    }

    private static class QueueReleasingHandler extends CountingHandler {

        QueueReleasingHandler() {
            super(RETRY_UNBOUNDED, 0);
        }

        @Override
        public void waitInMillis(long milliseconds) {
            super.waitInMillis(milliseconds);
            if (getRetryCount() == 2) {
                // Capacity becomes available while the handler is retrying.
                executor.getQueue().poll();
            }
        }

        private ThreadPoolExecutor executor;

        @Override
        public void rejectedExecution(Runnable r, ThreadPoolExecutor executor) {
            this.executor = executor;
            super.rejectedExecution(r, executor);
        }
    }
}
