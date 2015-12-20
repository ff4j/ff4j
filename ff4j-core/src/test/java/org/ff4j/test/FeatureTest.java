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

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import org.ff4j.core.Feature;
import org.ff4j.strategy.PonderationStrategy;
import org.junit.Assert;
import org.junit.Test;

/**
 * Testing Bean {@link Feature} initialization.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class FeatureTest {

    @Test(expected = IllegalArgumentException.class)
    public void testFeatureNameNull() {
        new Feature(null, false, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testFeatureNameEmpty() {
        new Feature("", false, null);
    }

    @Test
    public void testDescriptionNull() {
        Feature f = new Feature("ok", true, null);
        Assert.assertNotNull(f.toString(), "f should be serialized even without description");
        Assert.assertTrue(f.isEnable());
    }

    @Test
    public void testRolesEmpty() {
        Feature f2 = new Feature("ok2", true, null, null, new HashSet<String>());
        Assert.assertNotNull(f2.toString(), "f should be serialized even without roles");
        Assert.assertTrue(f2.isEnable());
    }

    @Test
    public void testFullToStringImpl() {
        List<String> auths = Arrays.asList(new String[] {"something"});
        Feature f = new Feature("ok", true, "grp1", "description", auths, new PonderationStrategy());
        Assert.assertTrue(f.toString().contains("ok"));
    }

    @Test
    public void testBuildFromScratchFeature() {
        Feature empty = new Feature("abc");
        empty.setUid("abc");

        // Flipping strategy
        empty.setFlippingStrategy(new PonderationStrategy());
        Assert.assertNotNull(empty.getFlippingStrategy());

        // Authorization filling
        List<String> auths = Arrays.asList(new String[] {"something"});
        empty.setPermissions(new HashSet<String>(auths));
        Assert.assertNotNull(empty.getPermissions());

        // Description setter
        empty.setDescription("OK");
        Assert.assertNotNull(empty.getDescription());

        // Toggle to change value
        empty.setEnable(true);
        empty.toggle();
        Assert.assertFalse(empty.isEnable());
        
        // GROUP
        empty.setGroup("sampleGroup");
        Assert.assertFalse(empty.getGroup() == null);

        // To String with a whole object
        Assert.assertTrue(empty.toString().contains("OK"));
    }

}
