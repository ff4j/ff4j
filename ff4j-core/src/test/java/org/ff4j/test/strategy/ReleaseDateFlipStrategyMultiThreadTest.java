package org.ff4j.test.strategy;

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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.ff4j.strategy.time.ReleaseDateFlipStrategy;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;

import static org.junit.Assert.assertEquals;


/**
 * Testing class for {@link ReleaseDateFlipStrategy} class.
 *
 * @author Henry Naftulin
 */
public class ReleaseDateFlipStrategyMultiThreadTest {
    protected final Log logger = LogFactory.getLog(getClass());

    public static String[] validDates = {
            "2023-01-01-10:22",
            "2023-01-01-10:23",
            "2023-01-01-10:24",
            "2023-01-01-10:25",
            "2023-01-01-10:26",
            "2023-01-01-10:27"
    };

    int numThreads = 40;

    private static CountDownLatch latch;

    private List<FeatureDateRunnable> runnables = new ArrayList<>();

    private static List<String> errors = new ArrayList<>();


    @Test
    public void testMultipleThreads() throws InterruptedException {
        for(int i=0; i<numThreads; i++) {
            runnables.add(new
                    FeatureDateRunnable(validDates[i%validDates.length]));
        }
        latch = new CountDownLatch(numThreads);
        for(Runnable r : runnables) {
            new Thread(r).start();
        }
        latch.await();
        logger.info("Ran "+ numThreads + " trials, got errors in "
                                        + errors.size() + " trials");
        assertEquals(new ArrayList<>(), errors);
    }


    static class FeatureDateRunnable implements Runnable {
        protected final Log logger = LogFactory.getLog(getClass());

        private String strDate;

        public FeatureDateRunnable(String strDate) {
            this.strDate = strDate;
        }

        public void run() {
            try {
                new ReleaseDateFlipStrategy(strDate);
            } catch(Throwable t) {
                String message = "Exception thrown when creating " +
                        "ReleaseDateFlipStrategy with date: " + strDate;
                errors.add(message);
                logger.error(message, t);
            } finally {
                latch.countDown();
            }
        }
    }
}
