package org.ff4j.test;

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

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import org.ff4j.core.Feature;
import org.ff4j.strategy.RandomFlipStrategy;
import org.junit.Assert;
import org.junit.Test;

/**
 * Testing Bean {@link Feature} initialization.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
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
        Feature f2 = new Feature("ok2", true, null, new HashSet<String>());
        Assert.assertNotNull(f2.toString(), "f should be serialized even without roles");
        Assert.assertTrue(f2.isEnable());
    }

    @Test
    public void testFullToStringImpl() {
        List<String> auths = Arrays.asList(new String[] {"something"});
        Feature f = new Feature("ok", true, "description", auths, new RandomFlipStrategy());
        Assert.assertTrue(f.toString().contains("ok"));
    }

    @Test
    public void testBuildFromScratchFeature() {
        Feature empty = new Feature("abc", false, null);
        List<String> auths = Arrays.asList(new String[] {"something"});
        empty.setUid("OK");
        empty.toggle();
        empty.setFlippingStrategy(new RandomFlipStrategy());
        empty.setAuthorizations(new HashSet<String>(auths));
        empty.setEnable(true);
        empty.setDescription("ok");
        empty.toString();
        Assert.assertTrue(empty.toString().contains("OK"));
        Assert.assertTrue(empty.isEnable());
    }

}
