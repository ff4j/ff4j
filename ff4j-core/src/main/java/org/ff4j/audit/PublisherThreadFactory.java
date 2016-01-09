package org.ff4j.audit;

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


import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Proposition of creation of new theards to enforce thread names.*
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PublisherThreadFactory implements ThreadFactory {
    
    /** parameters. */
    private final AtomicInteger poolNumber = new AtomicInteger(1);
    
    /** parameters. */
    private final ThreadGroup group;
    
    /** parameters. */
    private final AtomicInteger threadNumber = new AtomicInteger(1);
    
    /** parameters. */
    private final String namePrefix;

    /**
     * Default Constructor.
     */
    public PublisherThreadFactory() {
        group = Thread.currentThread().getThreadGroup();
        namePrefix = "ff4j-monitoring-pool-" + poolNumber.getAndIncrement() + "-thread-";
    }

    /** {@inheritDoc} */
    public Thread newThread(Runnable r) {
        Thread t = new Thread(group, r, namePrefix + threadNumber.getAndIncrement(), 0);
        t.setDaemon(false);
        t.setPriority(Thread.NORM_PRIORITY);
        return t;
    }
}
