package org.ff4j.test;

/*
 * #%L
 * ff4j-core
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Arrays;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Test;

/**
 * Test operations over {@link FF4j}
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jTest extends AbstractFf4jTest {

    @Override
    public FF4j initFF4j() {
        return new FF4j("ff4j.xml");
    }

    @Test
    public void helloWorldTest() {
        // Default : store = inMemory, load features (5) from ff4j.xml file
        assertEquals(5, ff4j.getFeatures().size());

        // Dynamically create feature and add it to the store (tests purpose)
        ff4j.create("sayHello");

        // Enable Feature
        ff4j.enable("sayHello");

        // Assertion
        assertTrue(ff4j.exist("sayHello"));
        assertEquals(6, ff4j.getFeatures().size());
        assertTrue(ff4j.check("sayHello"));
    }

    @Test
    public void autoCreateFeatureEnableTest() {

        // Default : store = inMemory, load features from ff4j.xml file
        FF4j ff4j = new FF4j("ff4j.xml");
        ff4j.setAutocreate(true);
        assertFalse(ff4j.exist("autoCreatedFeature"));

        // Auto creation by testing its value
        assertFalse(ff4j.check("autoCreatedFeature"));

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
        assertTrue(ff4j.check("f1"));
    }

    // enabling...

    @Test
    public void testEnableFeature() {
        FF4j ff4j = new FF4j();
        ff4j.autoCreate(true);
        ff4j.enable("newffff");
        Assert.assertTrue(ff4j.exist("newffff"));
        Assert.assertTrue(ff4j.check("newffff"));
    }

    @Test(expected = FeatureNotFoundException.class)
    public void testEnableFeatureNotExist() {
        ff4j.enable("newffff");
    }

    // disabling...

    @Test
    public void testDisableFeature() {
        FF4j ff4j = new FF4j();
        ff4j.autoCreate(true);
        ff4j.disable("newffff");
        Assert.assertTrue(ff4j.exist("newffff"));
        Assert.assertFalse(ff4j.check("newffff"));
    }

    @Test(expected = FeatureNotFoundException.class)
    public void testDisableFeatureNotExist() {
        FF4j ff4j = new FF4j();
        ff4j.disable("newffff");
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
        FF4j ff4j = new FF4j().autoCreate(true).create(
                new Feature("coco", true, "grp2", "", Arrays.asList(new String[] {"ROLEA"})));
        Assert.assertTrue(ff4j.check("coco"));
        ff4j.setAuthorizationsManager(mockAuthManager);
        Assert.assertTrue(ff4j.check("coco"));
        FlippingExecutionContext ex = new FlippingExecutionContext();
        ex.putString("OK", "OK");
        Assert.assertTrue(ff4j.check("coco", ex));
        Assert.assertTrue(ff4j.checkOveridingStrategy("coco", mockFlipStrategy));
        Assert.assertTrue(ff4j.checkOveridingStrategy("coco", null, null));
        Assert.assertFalse(ff4j.checkOveridingStrategy("cocorico", mockFlipStrategy));
    }
    
    @Test
    public void testToString() {
        Assert.assertTrue(ff4j.toString().contains(InMemoryFeatureStore.class.getCanonicalName()));
    }

    @Test
    public void testExportFeatures() throws IOException {
        Assert.assertNotNull(ff4j.exportFeatures());
    }



}
