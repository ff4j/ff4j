package org.ff4j.test;

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

    // log

    @Test
    public void testLogFeatures() {
        new FF4j().logFeatures();
    }

    @Test
    public void testStaticLogFeature() {
        FF4j.sLogFeatures();
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

    @Test
    public void testEnableFeatureStatic() {
        FF4j.sAutoCreateFeature(true);
        FF4j.sEnableFeature("newffff");
        Assert.assertTrue(FF4j.sExistFeature("newffff"));
        Assert.assertTrue(FF4j.sIsFlipped("newffff"));
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
    public void testDisableFeatureStatic() {
        FF4j.sAutoCreateFeature(true);
        FF4j.sDisableFeature("newffff");
        Assert.assertTrue(FF4j.sExistFeature("newffff"));
        Assert.assertFalse(FF4j.sIsFlipped("newffff"));
    }

    @Test
    public void testGetFeatures() {
        FF4j ff4j = new FF4j();
        Assert.assertEquals(5, ff4j.getFeatures().size());
    }

    @Test
    public void testGetFeaturesStatic() {
        Assert.assertEquals(6, FF4j.sGetFeatures().size());
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
