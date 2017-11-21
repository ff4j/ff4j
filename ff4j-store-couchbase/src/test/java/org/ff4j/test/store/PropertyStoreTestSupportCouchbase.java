package org.ff4j.test.store;

/*
 * #%L
 * ff4j-store-springcouchbase
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import org.ff4j.property.Property;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Assert;

import java.util.Iterator;
import java.util.Map;

abstract public class PropertyStoreTestSupportCouchbase extends PropertyStoreTestSupport {
    private void sleep() {
        for (int i = 0; i < 3; i++) {
            try {
                this.testedStore.readAllProperties();
                Thread.sleep(100);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public void readAllProperties() {
        sleep();
        sleep();
        sleep();
        Assert.assertNotNull(this.testedStore);
        Map<String, Property<?>> mapsOf = this.testedStore.readAllProperties();
        Assert.assertTrue(mapsOf.containsKey("a"));
        Assert.assertTrue(mapsOf.containsKey("b"));
    }

    @Override
    public void clear() {
        sleep();
        sleep();
        sleep();
        Assert.assertNotNull(this.testedStore);
        Map<String, Property<?>> before = this.testedStore.readAllProperties();
        Assert.assertFalse(before.isEmpty());
        this.testedStore.clear();
        Assert.assertTrue(this.testedStore.readAllProperties().isEmpty());
        Iterator var2 = before.entrySet().iterator();

        while(var2.hasNext()) {
            Map.Entry<String, Property<?>> pName = (Map.Entry)var2.next();
            this.testedStore.createProperty((Property)pName.getValue());
        }
        sleep();
        sleep();
        sleep();
    }
}
