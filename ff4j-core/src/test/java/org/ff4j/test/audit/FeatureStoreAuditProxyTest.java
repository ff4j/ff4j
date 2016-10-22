package org.ff4j.test.audit;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

import org.ff4j.FF4j;
import org.ff4j.audit.proxy.FeatureStoreAuditProxy;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.store.CoreFeatureStoreTestSupport;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;


public class FeatureStoreAuditProxyTest extends CoreFeatureStoreTestSupport {

    /** {@inheritDoc} */
    @Override
    public FeatureStore initStore() {
        FF4j ff4j = new FF4j();
        InMemoryFeatureStore imfs = new InMemoryFeatureStore();
        imfs.setLocation("ff4j.xml");
        ff4j.setFeatureStore(imfs);
        return new FeatureStoreAuditProxy(ff4j, imfs);
    }
    
    @Test
    public void testCreateSchema() {
        testedStore.createSchema();
    }
    
    @Test
    public void testImportFeatures() {
        testedStore.importFeatures(null);
        
        Feature fx1 = new Feature("fx1", true);
        Feature fx2 = new Feature("fx2", true);
        testedStore.importFeatures(Util.list(fx1, fx2));
        Assert.assertTrue(testedStore.exist("fx1"));
        testedStore.importFeatures(Util.list(fx1, fx2));
        
    }

}
