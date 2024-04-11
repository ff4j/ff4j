package org.ff4j.test;

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

import org.ff4j.FF4j;
import org.ff4j.conf.XmlParser;
import org.ff4j.core.Feature;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Assert;
import org.junit.Test;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.Stream;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * Test parallel operations with autocreate=true over {@link FF4j}
 * 
 * @author Mariusz Zawadzki ( mariusz.r.zawadzki@gmail.com )
 */
public class FF4j_autocreate_multithreaded_Test extends AbstractFf4jTest {

    private ExecutorService executor = Executors.newFixedThreadPool(2);

    @Override
    public FF4j initFF4j() {
        FF4j ff4j = new FF4j(new XmlParser(), "ff4j.xml");
        ff4j.setFeatureStore(new DelayingFeatureStore("ff4j.xml"));
        return ff4j;
    }

    @Test
    public void autoCreateFeatureCheckTest() {

        // Default : store = inMemory, load features from ff4j.xml file
        ff4j.autoCreate();
        assertFalse(ff4j.exist("autoCreatedFeature"));

        testParallel(() -> ff4j.check("autoCreatedFeature"), Assert::assertFalse);

        // Assertion
        assertTrue(ff4j.exist("autoCreatedFeature"));
    }

    // enabling
    @Test
    public void autoCreateFeatureEnableTest() {

        // Default : store = inMemory, load features from ff4j.xml file
        ff4j.autoCreate();
        assertFalse(ff4j.exist("autoCreatedFeature"));

        testParallel(() -> ff4j.enable("autoCreatedFeature"), it-> Assert.assertEquals(it, ff4j));

        // Assertion
        assertTrue(ff4j.exist("autoCreatedFeature"));
    }

    // disabling
    @Test
    public void autoCreateFeatureDisnableTest() {

        // Default : store = inMemory, load features from ff4j.xml file
        ff4j.autoCreate();
        assertFalse(ff4j.exist("autoCreatedFeature"));

        testParallel(() -> ff4j.disable("autoCreatedFeature"), it-> Assert.assertEquals(it, ff4j));

        // Assertion
        assertFalse(ff4j.check("autoCreatedFeature"));
    }

    private <T> void testParallel(Callable<T> testedMethod, Consumer<T> assertions) {
        // Auto creation by testing its value
        Future<T> firstAutoCreate  = executor.submit(testedMethod);
        Future<T> secondAutoCreate = executor.submit(testedMethod);
        Stream.of(firstAutoCreate, secondAutoCreate)
                .map(it-> {
                    try {
                        return it.get();
                    } catch (ExecutionException | InterruptedException e) {
                        throw new RuntimeException(e);
                    }
                }).forEach(assertions);
    }

    static class DelayingFeatureStore extends InMemoryFeatureStore{

        AtomicInteger counter = new AtomicInteger(1);
        static final long BASE_DELAY = 100L;

        DelayingFeatureStore(String fileName) {
            super(fileName);
        }
        @Override
        public void create(Feature fp) {
            try {
                Thread.sleep(BASE_DELAY + increaseDelay());
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            super.create(fp);
        }

        private long increaseDelay() {
            return 10L*counter.getAndIncrement();
        }
    }
}
