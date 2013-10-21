package org.ff4j.test.gettingstarted;

/*
 * #%L
 * ff4j-core
 * $Id:$
 * $HeadURL:$
 * %%
 * Copyright (C) 2013 Ff4J
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.ff4j.FF4j;
import org.ff4j.exception.FeatureNotFoundException;
import org.junit.Test;

/**
 * Those code snipplete are used as samples and eventually propose in documentation
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class GettingStartedCode {

    @Test
    public void testingFeatures() {
        FF4j ff4j = new FF4j();
        ff4j.create("f1", true);
        System.out.println(ff4j);

        // Test UP
        if (ff4j.isFlipped("f1")) {
            System.out.println("Hello the feature is enabled");
        }

        // Test DOWN
        ff4j.disable("f1");

        if (ff4j.isFlipped("f1")) {
            System.out.println("Hello the feature is enabled");
        }

    }

    @Test
    public void helloWorld() {

        FF4j ff4j = new FF4j("ff4j.xml");

        // Work with it
        if (ff4j.isFlipped("AwesomeFeature")) {
            System.out.println("Hello the feature is enabled");
        }

        // Its does not exist
        try {
            if (ff4j.isFlipped("do-not-exit")) {}
        } catch (FeatureNotFoundException fnfe) {
            System.out.println(fnfe.getMessage());
        }

        ff4j.autoCreate(true);
        // no more exception but returning false
        if (ff4j.isFlipped("do-not-exit")) {}

    }

    @Test
    public void autoCreateFeatureEnableTest() {

        // Default : store = inMemory, load features from ff4j.xml file
        FF4j ff4j = new FF4j("ff4j.xml");
        ff4j.setAutocreate(true);
        assertFalse(ff4j.exist("autoCreatedFeature"));

        // Auto creation by testing its value
        assertFalse(ff4j.isFlipped("autoCreatedFeature"));

        // Assertion
        assertTrue(ff4j.exist("autoCreatedFeature"));
    }

    @Test
    public void workingWithFeature() {

        // Initialize with empty store
        FF4j ff4j = new FF4j();

        // Dynamically register new features
        ff4j.create("f1").enable("f1");

        // Assertions
        assertTrue(ff4j.exist("f1"));
        assertTrue(ff4j.isFlipped("f1"));
    }

}
