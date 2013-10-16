package org.ff4j.test.gettingstarted;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.ff4j.FF4j;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Test;

public class GettingStartedCode {

    @Test
    public void helloWorldTest() {

        // Default : store = inMemory, load features (5) from ff4j.xml file
        FF4j ff4j = new FF4j("ff4j.xml");
        assertEquals(5, ff4j.getFeatures().size());

        // Dynamically create feature and add it to the store (tests purpose)
        ff4j.createFeature("sayHello");
        assertTrue(ff4j.existFeature("sayHello"));
        assertEquals(6, ff4j.getFeatures().size());

        // Enable Feature
        ff4j.enableFeature("sayHello");
        assertTrue(ff4j.isFlipped("sayHello"));

        // Test Value at runtime
        if (ff4j.isFlipped("sayHello")) {
            System.out.println("Hello World !");
        } else {
            fail();
        }
    }

    @Test
    public void autoCreateFeatureEnableTest() {

        // Default : store = inMemory, load features (5) from ff4j.xml file
        FF4j ff4j = new FF4j("ff4j.xml");
        ff4j.setAutocreate(true);

        if (!ff4j.isFlipped("autoCreatedFeature")) {
            System.out.println("autoCreatedFeature is created but not available");
        } else {
            fail();
        }
    }

    @Test
    public void workingWithFeature() {

        // Initialize with empty store
        FF4j ff4j = new FF4j(new InMemoryFeatureStore());

        // Dynamically register new features
        ff4j.createFeature("f1").enableFeature("f1");

        // Testing
        assertTrue(ff4j.existFeature("f1"));
        assertTrue(ff4j.isFlipped("f1"));

    }

}
