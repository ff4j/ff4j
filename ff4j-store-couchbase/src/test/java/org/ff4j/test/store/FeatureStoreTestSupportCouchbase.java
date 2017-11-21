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

import org.ff4j.core.Feature;
import org.junit.Assert;

import java.util.*;

abstract public class FeatureStoreTestSupportCouchbase extends FeatureStoreTestSupport {
    @Override
    protected int enablePause() {
        return 1;
    }

    private void sleep() {
        for (int i = 0; i < 3; i++) {
            try {
                this.testedStore.readAll();
                Thread.sleep(100);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public void testEnableGroup() {
        sleep();
        this.testedStore.disable("second");
        this.testedStore.addToGroup("second", "GRP0");
        this.assertFf4j.assertThatFeatureIsDisabled("second");
        this.assertFf4j.assertThatFeatureIsInGroup("second", "GRP0");
        this.testedStore.enableGroup("GRP0");
        this.assertFf4j.assertThatFeatureIsEnabled("second");
        this.testedStore.disable("second");
    }

    @Override
    public void testClear() {
        Assert.assertNotNull(this.testedStore);
        Map<String, Feature> before = this.testedStore.readAll();
        Assert.assertFalse(before.isEmpty());
        this.testedStore.clear();
        sleep();
        Assert.assertTrue(this.testedStore.readAll().isEmpty());
        Iterator var2 = before.entrySet().iterator();

        while(var2.hasNext()) {
            Map.Entry<String, Feature> pName = (Map.Entry)var2.next();
            this.testedStore.create((Feature)pName.getValue());
        }
        sleep();
        sleep();
        sleep();
    }

    @Override
    public void testAddFeature() throws Exception {
        this.assertFf4j.assertThatFeatureDoesNotExist("new");
        Set<String> rights = new HashSet(Arrays.asList("USER"));
        Feature fp = new Feature("new", true, "description", "GRP1", rights);
        this.testedStore.create(fp);
        sleep();
        this.assertFf4j.assertThatStoreHasSize(6);
        this.assertFf4j.assertThatFeatureExist("new");
        this.assertFf4j.assertThatFeatureIsInGroup("new", "GRP1");
        this.testedStore.delete("new");
        this.assertFf4j.assertThatFeatureDoesNotExist("new");
    }

    @Override
    public void testRemoveFromGroup() {
        this.assertFf4j.assertThatGroupHasSize(2, "GRP1");
        this.testedStore.removeFromGroup("third", "GRP1");
        this.assertFf4j.assertThatGroupHasSize(1, "GRP1");
        this.testedStore.addToGroup("third", "GRP1");
        this.assertFf4j.assertThatGroupHasSize(2, "GRP1");
    }
}
