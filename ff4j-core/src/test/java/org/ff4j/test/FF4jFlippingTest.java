package org.ff4j.test;

import org.ff4j.FF4j;
import org.junit.Test;

public class FF4jFlippingTest {

    @Test
    public void testLogFeatures() {
        FF4j f5 = new FF4j();
        f5.logFeatures();
        FF4j.sLogFeatures();
    }

    public void testEnableFeature() {}

    public void testEnableFeatureDoesNotExist() {}

    public void testEnableFatureDoesNotExistAutoCreate() {}

    public void testGetFeatures() {}

    public void testGetFeatures_WithoutInitialization() {}

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
