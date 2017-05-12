package org.ff4j.test.store;

/*
 * #%L
 * ff4j-test
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

import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.cache.AbstractCacheManagerJUnitTest;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Assert;
import org.junit.Test;

/**
 * Enhance code coverage in limit use cases.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class AbstractCacheManagerTest {
    
    public static FF4JCacheManager cacheManager;
    
    @Test
    public void testAbstractCacheManagerForCoverage() {
        AbstractCacheManagerJUnitTest ac = new AbstractCacheManagerJUnitTest() {
            protected FF4JCacheManager getCacheManager() {
                if (cacheManager == null) {
                    cacheManager = new InMemoryCacheManager();   
                }
                return cacheManager;
            }
        };
        ac.initialize();
        ac.testClear();
        ac.testPutOK();
        ac.clean();
        ac.testPutSeveral();
        ac.testEvictOK();
        ac.clean();
        ac.testPutAvoidDoublon();
        ac.clean();
        ac.testEvictFeatureNotExist();
        ac.clean();
        Assert.assertNotNull(ac);
    }
    
    @Test
    public void testPropertyStoreCoverage() throws Exception {
        // enhance coverage by not throwing exception on java method
        PropertyStoreTestSupport ps = new PropertyStoreTestSupport() {
            protected PropertyStore initPropertyStore() {
                return new MockPropertyStore();
            }
        };
        ps.setUp();
        ps.existKONull();
        ps.existKOEmpty();
        ps.addPropertyKONull();
        ps.readKOnull();
        ps.readKOempty();
        ps.readKOnotExist();
        ps.addPropertyKOAlreadyExist();
        ps.deleteKOnull();
        ps.deleteKOempty();
        ps.deleteKOdoesnotexist();
        ps.existfilled();
        ps.valueFixed();
        ps.clear();
        ps.updateKOempty();
        ps.updateKOnull();
        ps.updateKOdoesnotExist();
        ps.updateKOPropertyNotFound();
        ps.updateKOInvalidValue();
        ps.updateKONullBis();
        Assert.assertNotNull(ps);
    }
    
    @Test
    public void testFeatureStoreCoverage() throws Exception {
        // enhance coverage by not throwing exception on java method
        FeatureStoreTestSupport ps = new FeatureStoreTestSupport() {
            protected FeatureStore initStore() {
                return new MockFeatureStore();
            }
        };
        ps.setUp();
        
        ps.testDonotDeleteEmpty();
        ps.testDonotDeleteNull();
        ps.testDonotUpdateNullFeature();
        ps.testRemoveToGroupEmpty();
        ps.testRemoveToGroupNull();
        ps.testRemoveToGroupFeatureNull();
        ps.testAddToGroupFeatureDoeNotExist();
        ps.testDeleteNull();
        ps.testDeteleFeatureDoesnotExist();
        ps.testGrantRoleNullFeature();
        ps.testGrantRoleEmptyFeature();
        ps.testGrantRoleNullRole();
        ps.testGrantRoleEmptyRole();
        ps.testGrantRoleToFeatureFeatureDoesNotExist();
        ps.testRemoveRoleNullFeature();
        ps.testRemoveRoleEmptyFeature();
        ps.testRemoveRoleNullRole();
        ps.testRemoveRoleEmptyRole();
        ps.testUpdateNull();
        ps.testFeatureDoesNotExit();
        ps.testDeleteRoleFeatureDoesNotExit();
        ps.testReadNull();
        ps.testReadEmpty();
        ps.testReadNotExist();
        ps.testEnableNull();
        ps.testEnableEmpty();
        ps.testEnableFeatureDoesNotExist();
        ps.testDisableNull();
        ps.testDisableEmpty();
        ps.testDisableFeatureDoesNotExist();
        ps.testCreateNull();
        ps.testExistNull();
        ps.testExistEmpty();
        ps.testExistGroupNull();
        ps.testExistGroupEmpty();
        ps.testEnableGroupNull();
        ps.testEnableGroupEmpty();
        ps.testEnableGroupDoesNotExist();
        ps.testDisableGroupNull();
        ps.testDisableGroupEmpty();
        ps.testDisableGroupDoesNotExist();
        ps.testReadGroupNull();
        ps.testReadGroupEmpty();
        ps.testReadGroupDoesnotExist();
        ps.testAddToGroupFeatureNull();
        ps.testAddToGroupFeatureEmpty();
        ps.testAddToGroupNull();
        ps.testAddToGroupEmpty();
        ps.testRemoveToGroupFeatureEmpty();
        ps.testRemoveFromGroupDoesNotExist();
        ps.testRemoveFromGroupFeatureDoeNotExist();
        ps.testClear();
        Assert.assertNotNull(ps);
    }
    
    @Test(expected = AssertionError.class)
    public void testError() throws Exception {
        FeatureStoreTestSupport ps = new FeatureStoreTestSupport() {
            protected FeatureStore initStore() {
                return new MockFeatureStore();
            }
        };
        ps.setUp();
        ps.testUpdateEditPropertyValue();
    }
}
