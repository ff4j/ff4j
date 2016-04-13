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

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.property.PropertyInt;
import org.ff4j.property.PropertyString;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.ff4j.test.TestsFf4jConstants.*;

/**
 * Testing JDBC Store with spring ans conf as XML.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:*applicationContext-jdbc-test.xml")
public class SpringJdbcXMLDataSourceStoreTest extends FeatureStoreTestSupport {
   
    @Autowired
    private FeatureStore store;

    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        return store;
    }

    /**
     * TDD.
     */
    @Test
    @Override
    public void testStoreHasBeenInitialized() {
        // Given
        assertFf4j.assertThatStoreHasSize(EXPECTED_FEATURES_NUMBERS);
        assertFf4j.assertThatFeatureFlipped(F1);
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

    /**
     * TDD.
     */
    @Test
    @Override
    public void testUpdateFlipLessAutorisation() {
        // Given
        assertFf4j.assertThatFeatureExist(F1);
        assertFf4j.assertThatFeatureHasRole(F1, ROLE_USER);
        // When
        testedStore.update(new Feature(F1, false, null));
        // Then
        assertFf4j.assertThatFeatureHasNotRole(F1, ROLE_USER);
    }

    /**
     * TDD.
     */
    @Test
    @Override
    public void testGrantRoleToFeatureRoleDoesNotExist() throws Exception {
        // Given
        assertFf4j.assertThatFeatureExist(F1);
        assertFf4j.assertThatFeatureHasNotRole(F1, ROLE_NEW);
        // When
        testedStore.grantRoleOnFeature(F1, ROLE_NEW);
        // Then
        assertFf4j.assertThatFeatureHasRole(F1, ROLE_NEW);
    }
    
    /**
     * TDD.
     */
    @Test
    @Override
    @SuppressWarnings("unchecked")
    public void testUpdateEditPropertyAddFixedValues() {
        // Given
        assertFf4j.assertThatFeatureExist(F1);
        Feature myFeature = ff4j.getFeatureStore().read(F1);
        myFeature = ff4j.getFeatureStore().read(F1);
        assertFf4j.assertThatFeatureHasProperty(F1, "digitValue");
        
        Set < Integer > fixValues = (Set<Integer>) ff4j
                .getFeatureStore().read(F1)//
                .getCustomProperties().get("digitValue")
                .getFixedValues();
        Assert.assertEquals(4, fixValues.size()); 
                
        // When
        PropertyInt p1 = new PropertyInt("digitValue");
        p1.setFixedValues(Util.set(0,1,2,3,4));
        p1.setValue(4);
        myFeature.getCustomProperties().put(p1.getName(), p1);
        testedStore.update(myFeature);
        
        // Then
        Set < Integer > fixValues2 = (Set<Integer>) ff4j
                .getFeatureStore().read(F1)//
                .getCustomProperties().get("digitValue")
                .getFixedValues();
        Assert.assertEquals(5, fixValues2.size());
    }
    
    /**
     * TDD.
     */
    @Test
    @Override
    @SuppressWarnings("unchecked")
    public void testUpdateEditPropertyRemoveFixedValues() {
     // Given
        assertFf4j.assertThatFeatureExist(F1);
        assertFf4j.assertThatFeatureHasProperty(F1, "regionIdentifier");
        Set < String > fixValues = (Set<String>) ff4j
                .getFeatureStore().read(F1)//
                .getCustomProperties().get("regionIdentifier")
                .getFixedValues();
        Assert.assertEquals(3, fixValues.size()); 
                
        // When
        Feature myFeature = ff4j.getFeatureStore().read(F1);
        PropertyString p1 = new PropertyString("regionIdentifier");
        p1.setValue("AMER");
        p1.setFixedValues(Util.set("AMER", "SSSS"));
        myFeature.getCustomProperties().put(p1.getName(), p1);
        testedStore.update(myFeature);
        
        // Then
        Set < Integer > fixValues2 = (Set<Integer>) ff4j
                .getFeatureStore().read(F1)//
                .getCustomProperties().get("regionIdentifier")
                .getFixedValues();
        Assert.assertEquals(2, fixValues2.size());
    }
}