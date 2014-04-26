package org.ff4j.test.store;

/*
 * #%L ff4j-store-jdbc %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.Set;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureNotFoundException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Testing JDBC Store with spring ans conf as XML.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:*applicationContext-jdbc-test.xml")
public class SpringJdbcXMLDataSourceStoreTest extends AbstractStoreJUnitTest {

    @Autowired
    private FF4j ff4j;

    @Autowired
    private FeatureStore store;

    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        return store;
    }

    /**
     * TDD : Exact same code from upper class
     */
    @Test(expected = FeatureNotFoundException.class)
    @Override
    public void testRemoveFromGroupFeatureDoeNotExist() {
        // Given
        assertFf4j.assertThatGroupExist(G1);
        assertFf4j.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        testedStore.removeFromGroup(F_DOESNOTEXIST, G1);
        // Then, expected error
    }

    /**
     * TDD.
     */
    @Test
    @Override
    public void testReadAllGroup() {
        // Given
        assertFf4j.assertThatGroupExist(G0);
        assertFf4j.assertThatGroupExist(G1);
        // When
        Set<String> groups = testedStore.readAllGroups();
        // Then
        Assert.assertEquals(2, groups.size());
        Assert.assertTrue(groups.contains(G0));
        Assert.assertTrue(groups.contains(G1));
    }
}