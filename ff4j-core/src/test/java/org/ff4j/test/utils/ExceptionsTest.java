package org.ff4j.test.utils;

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

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.EventRejectedExecutionHandler;
import org.ff4j.audit.PublisherThreadFactory;


import org.ff4j.exception.AuditAccessException;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.PropertyAccessException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Enhance test coverage by raising exceptions used in other modules.
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
public class ExceptionsTest {
    
    @Test
    public void testFeatureAccessException() {
        assertThrows(FeatureAccessException.class, () -> {
            throw new FeatureAccessException("Can be triggered with single message");
        });
    }
    
    @Test
    public void testPropertyAccessException() {
        assertThrows(PropertyAccessException.class, () -> {
            throw new PropertyAccessException("Can be triggered with single message");
        });
    }
    
    @Test
    public void testPropertyAccessException2() {
        assertThrows(PropertyAccessException.class, () -> {
            throw new PropertyAccessException("Can be triggered with single message", new IllegalArgumentException());
        });
    }
    
    @Test
    public void testAuditAccessException() {
        assertThrows(AuditAccessException.class, () -> {
            throw new AuditAccessException("Can be triggered with single message");
        });
    }
    
    @Test
    public void testAuditAccessException2() {
        assertThrows(AuditAccessException.class, () -> {
            throw new AuditAccessException("Can be triggered with single message", new IllegalArgumentException());
        });
    }

    @Test
    public void testFeatureNotFound() {
        assertThrows(FeatureNotFoundException.class, () -> {
            throw new FeatureNotFoundException("Can be triggered with single message", new IllegalArgumentException());
        });
    }
    
    @Test
    public void testExceptionHandler() {
        EventRejectedExecutionHandler ereh = new EventRejectedExecutionHandler();
        final BlockingQueue<Runnable> queue = new ArrayBlockingQueue<Runnable>(2);
        // Executor with worker to process threads
        RejectedExecutionHandler rej = new EventRejectedExecutionHandler();
        ThreadFactory tFactorty = new PublisherThreadFactory();
        ThreadPoolExecutor executor = 
                new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, queue, tFactorty, rej);
        
        ereh.rejectedExecution(new Thread(), executor);
    }
    
    @Test
    public void testExceptionHandlerInterrupted() throws InterruptedException {
        EventRejectedExecutionHandler.setMock(true);
        EventRejectedExecutionHandler ereh = new EventRejectedExecutionHandler();
        ereh.rejectedExecution(new Thread(), null);
        EventRejectedExecutionHandler.setMock(false);
        Assertions.assertNotNull(ereh);
    }
    
   
}
