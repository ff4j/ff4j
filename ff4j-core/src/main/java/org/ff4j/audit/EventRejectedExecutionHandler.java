package org.ff4j.audit;

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

import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * Handle Rejects when publishing event.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class EventRejectedExecutionHandler implements RejectedExecutionHandler {

    /** Default delay between retries. */
    public static final long DEFAULT_RETRY_DELAY = 1000L;

    /** Default maximum retries for the bounded strategy. */
    public static final int DEFAULT_MAX_RETRIES = 3;

    /** Available strategies when the event publisher executor is saturated. */
    public enum RejectionStrategy {
        /** Retry until the task can be queued. */
        RETRY_UNBOUNDED,
        /** Retry up to the configured maximum. */
        RETRY_BOUNDED,
        /** Cancel the rejected task immediately. */
        DISCARD
    }

    /** Rejection strategy. */
    private final RejectionStrategy strategy;

    /** Maximum retries for the bounded strategy. */
    private final int maxRetries;

    /** Delay between retries. */
    private final long retryDelay;
 
    /** Simulate Interrupted. */
    private static boolean mock = false;

    /** Create a handler with the legacy unbounded retry behavior. */
    public EventRejectedExecutionHandler() {
        this(RejectionStrategy.RETRY_UNBOUNDED, 0, DEFAULT_RETRY_DELAY);
    }

    /**
     * Create a handler with default retry settings.
     *
     * @param strategy rejection strategy
     */
    public EventRejectedExecutionHandler(RejectionStrategy strategy) {
        this(strategy, DEFAULT_MAX_RETRIES, DEFAULT_RETRY_DELAY);
    }

    /**
     * Create a configurable rejection handler.
     *
     * @param strategy rejection strategy
     * @param maxRetries maximum retries for {@link RejectionStrategy#RETRY_BOUNDED}
     * @param retryDelay delay in milliseconds between retries
     */
    public EventRejectedExecutionHandler(RejectionStrategy strategy, int maxRetries, long retryDelay) {
        if (strategy == null) {
            throw new IllegalArgumentException("Rejection strategy cannot be null");
        }
        if (maxRetries < 0) {
            throw new IllegalArgumentException("Maximum retries cannot be negative");
        }
        if (retryDelay < 0) {
            throw new IllegalArgumentException("Retry delay cannot be negative");
        }
        this.strategy = strategy;
        this.maxRetries = maxRetries;
        this.retryDelay = retryDelay;
    }
    
    /** {@inheritDoc} */
    @Override
    public void rejectedExecution(Runnable r, ThreadPoolExecutor executor) {
        if (strategy == RejectionStrategy.DISCARD) {
            cancel(r);
            return;
        }

        int retryCount = 0;
        while (strategy == RejectionStrategy.RETRY_UNBOUNDED || retryCount < maxRetries) {
            try {
                waitInMillis(retryDelay);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                cancel(r);
                return;
            }
            if (executor.isShutdown()) {
                cancel(r);
                return;
            }
            if (executor.getQueue().offer(r)) {
                if (executor.isShutdown() && executor.remove(r)) {
                    cancel(r);
                }
                return;
            }
            retryCount++;
        }
        cancel(r);
    }

    private void cancel(Runnable runnable) {
        if (runnable instanceof Future<?>) {
            ((Future<?>) runnable).cancel(false);
        }
    }
    
    /**
     * Wait, extracted method to easy tests.
     *
     * @param nbSecond
     *      number of seconds to wait.
     * @throws InterruptedException
     *      interupted
     */
    public void waitInSeconds(int nbSecond) throws InterruptedException {
        waitInMillis(1000L * nbSecond);
    }

    /**
     * Wait between retries.
     *
     * @param milliseconds number of milliseconds to wait
     * @throws InterruptedException interrupted
     */
    public void waitInMillis(long milliseconds) throws InterruptedException {
        if (mock) throw new InterruptedException();
        Thread.sleep(milliseconds);
    }

    public static boolean isMock() {
        return mock;
    }

    public static void setMock(boolean mock) {
        EventRejectedExecutionHandler.mock = mock;
    }

}
