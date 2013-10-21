package org.ff4j.test;

/*
 * #%L ff4j-core $Id:$ $HeadURL:$ %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import junit.framework.Assert;

import org.ff4j.FF4j;
import org.ff4j.exception.FeatureNotFoundException;
import org.junit.Test;

/**
 * Test operations over {@link FF4j}
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jFlippingTest {

    @Test
    public void helloWorldTest() {

        // Default : store = inMemory, load features (5) from ff4j.xml file
        FF4j ff4j = new FF4j("ff4j.xml");
        assertEquals(5, ff4j.getFeatures().size());

        // Dynamically create feature and add it to the store (tests purpose)
        ff4j.createFeature("sayHello");

        // Enable Feature
        ff4j.enableFeature("sayHello");

        // Assertion
        assertTrue(ff4j.existFeature("sayHello"));
        assertEquals(6, ff4j.getFeatures().size());
        assertTrue(ff4j.isFlipped("sayHello"));
    }

    @Test
    public void autoCreateFeatureEnableTest() {

        // Default : store = inMemory, load features from ff4j.xml file
        FF4j ff4j = new FF4j("ff4j.xml");
        ff4j.setAutocreate(true);
        assertFalse(ff4j.existFeature("autoCreatedFeature"));

        // Auto creation by testing its value
        assertFalse(ff4j.isFlipped("autoCreatedFeature"));

        // Assertion
        assertTrue(ff4j.existFeature("autoCreatedFeature"));
    }

    @Test
    public void workingWithFeature() {

        // Initialize with empty store
        FF4j ff4j = new FF4j();

        // Dynamically register new features
        ff4j.createFeature("f1").enableFeature("f1");

        // Assertions
        assertTrue(ff4j.existFeature("f1"));
        assertTrue(ff4j.isFlipped("f1"));
    }

    // log

    @Test
    public void testLogFeatures() {
        new FF4j().logFeatures();
    }

    // enabling...

    @Test
    public void testEnableFeature() {
        FF4j ff4j = new FF4j();
        ff4j.autoCreateFeature(true);
        ff4j.enableFeature("newffff");
        Assert.assertTrue(ff4j.existFeature("newffff"));
        Assert.assertTrue(ff4j.isFlipped("newffff"));
    }

    @Test(expected = FeatureNotFoundException.class)
    public void testEnableFeatureNotExist() {
        FF4j ff4j = new FF4j();
        ff4j.enableFeature("newffff");
    }

    // disabling...

    @Test
    public void testDisableFeature() {
        FF4j ff4j = new FF4j();
        ff4j.autoCreateFeature(true);
        ff4j.disableFeature("newffff");
        Assert.assertTrue(ff4j.existFeature("newffff"));
        Assert.assertFalse(ff4j.isFlipped("newffff"));
    }

    @Test(expected = FeatureNotFoundException.class)
    public void testDisableFeatureNotExist() {
        FF4j ff4j = new FF4j();
        ff4j.disableFeature("newffff");
    }

    @Test
    public void testGetFeatures() {
        FF4j ff4j = new FF4j("ff4j.xml");
        Assert.assertEquals(5, ff4j.getFeatures().size());
    }

    public void testGetFeature() {}

    public void testGetFeature_DoesNotExist() {}

    public void testGetFeature_DoesNotExistAutoCreate() {}

    @Test
    public void testFlipped() {
        // FF4j ff4j = new FF4j();

        /*
         * Static Access FF4j.sLogFeatures(); FF4j.isFlipped("first"); f.setAuthorizationsManager(new
         * DefaultAuthorisationManager()); FF4j.isFlipped("first", new RandomFlipStrategy(), "test");
         * FF4j.getFeature("first").setFlippingStrategy(new RandomFlipStrategy()); FF4j.isFlipped("first", new
         * RandomFlipStrategy(), "test");
         * 
         * FF4j.getFeature("first").setEnable(false); FF4j.isFlipped("first"); FF4j.isFlipped("first", new RandomFlipStrategy(),
         * "test"); FF4j.getFeature("first").setFlippingStrategy(new RandomFlipStrategy()); FF4j.isFlipped("first", new
         * RandomFlipStrategy(), "test");
         */

    }

}
