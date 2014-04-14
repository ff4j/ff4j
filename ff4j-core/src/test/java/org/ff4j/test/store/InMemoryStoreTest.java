package org.ff4j.test.store;

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

import java.util.LinkedHashMap;

import junit.framework.Assert;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.PonderationFlipStrategy;
import org.ff4j.test.AbstractStoreTest;
import org.junit.Test;

/**
 * All TEST LOGIC is in super class to be processed on EACH STORE.
 * 
 * @author clunven
 */
public class InMemoryStoreTest extends AbstractStoreTest {

    /** {@inheritDoc} */
    @Override
    public FeatureStore initStore() {
        InMemoryFeatureStore imfs = new InMemoryFeatureStore();
        imfs.setLocation("ff4j.xml");
        return imfs;
    }

    @Test
    public void testUnitFeatureInitialization() {
        InMemoryFeatureStore imfs = new InMemoryFeatureStore();
        imfs.create(new Feature("default", true, "grp1", "desc", null, new PonderationFlipStrategy()));
        Assert.assertEquals(1, imfs.readAll().size());
    }

    @Test
    public void testUnitFeatureInitialization2() {
        LinkedHashMap<String, Feature> map1 = new LinkedHashMap<String, Feature>();
        map1.put("new", new Feature("new", true, "description"));
        map1.put("old", new Feature("old", true, "description"));
        InMemoryFeatureStore imfs = new InMemoryFeatureStore(map1);
        Assert.assertEquals(2, imfs.readAll().size());
        Assert.assertTrue(imfs.toString().contains("old"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testUnitFeatureInitialization3() {
        new InMemoryFeatureStore("invalid.xml");
    }

}
