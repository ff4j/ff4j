package org.ff4j.test.store;

/*
 * #%L ff4j-core %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlipStrategy;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.strategy.PonderationFlipStrategy;
import org.ff4j.test.AssertFf4j;
import org.ff4j.test.TestConstantsFF4j;
import org.junit.Before;
import org.junit.Test;

/**
 * For different store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class AbstractStoreTest implements TestConstantsFF4j {

    /** Initialize */
    protected FF4j ff4j = null;

    /** Tested Store. */
    protected FeatureStore testedStore;

    /** Test Values */
    protected AssertFf4j assertFf4j;

    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        ff4j = new FF4j();
        ff4j.setStore(initStore());
        testedStore = ff4j.getStore();
        assertFf4j = new AssertFf4j(ff4j);
    }

    /**
     * Any store test will declare its store through this callback.
     * 
     * @return working feature store
     * @throws Exception
     *             error during building feature store
     */
    protected abstract FeatureStore initStore();

    /**
     * Test that the store has been initialized.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testStoreHasBeenInitialized() throws Exception {
        // Given Initialization, Then
        assertFf4j.assertThatStoreHasSize(EXPECTED_FEATURES_NUMBERS);
        assertFf4j.assertThatFeatureFlipped(F1);
    }

    /**
     * Test that features have been correctle stored.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testReadllFeatures() {
        // Given
        assertFf4j.assertThatFeatureExist(F1);
        assertFf4j.assertThatFeatureExist(F4);
        // When
        Map<String, Feature> features = testedStore.readAll();
        // Then
        Assert.assertTrue(features.containsKey(F1));
        Assert.assertTrue(features.containsKey(F4));
    }

    /**
     * Test that features have been correctle stored.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testReadFullFeature() {
        // Given
        assertFf4j.assertThatFeatureExist(F4);
        // When
        Feature f = testedStore.read(F4);
        // Then
        Assert.assertEquals(f.getUid(), F4);
        Assert.assertTrue(f.getDescription() != null && !"".equals(f.getDescription()));
        Assert.assertTrue(f.getAuthorizations() != null && !f.getAuthorizations().isEmpty());
        assertFf4j.assertThatFeatureHasRole(F4, ROLE_ADMIN);
        assertFf4j.assertThatFeatureIsInGroup(F4, G1);
    }

    /**
     * Test that working with unknown feature throw exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testFlipFeatureDoesNotExist() {
        ff4j.isFlipped(F_DOESNOTEXIST);
    }

    /**
     * Test that enable unknown feature throw exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testEnableFeatureDoesNotExist() {
        ff4j.enable(F_DOESNOTEXIST);
    }

    /**
     * Test that disable unknown feature throw exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testDisableFeatureDoesNotExist() {
        ff4j.disable(F_DOESNOTEXIST);
    }

    /**
     * Test that enable is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testEnableFeature() {
        // When
        ff4j.enable(F1);
        // Then
        assertFf4j.assertThatFeatureFlipped(F1);
    }

    /**
     * Test that disable is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testDisableFeature() {
        // When
        ff4j.disable(F1);
        // Then
        assertFf4j.assertThatFeatureNotFlipped(F1);
    }

    /**
     * Test that addFeature is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testAddFeature() throws Exception {
        // Given
        assertFf4j.assertThatFeatureDoesNotExist(FEATURE_NEW);
        // When
        Set<String> rights = new HashSet<String>(Arrays.asList(new String[] {ROLE_USER}));
        Feature fp = new Feature(FEATURE_NEW, true, "description", G1, rights);
        testedStore.create(fp);
        // Then
        assertFf4j.assertThatStoreHasSize(EXPECTED_FEATURES_NUMBERS + 1);
        assertFf4j.assertThatFeatureExist(FEATURE_NEW);
        assertFf4j.assertThatFeatureIsInGroup(FEATURE_NEW, G1);
        // End, return to initial state
        testedStore.delete(FEATURE_NEW);
        assertFf4j.assertThatFeatureDoesNotExist(FEATURE_NEW);
    }

    /**
     * Test that creating feature already exist through exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureAlreadyExistException.class)
    public void testAddFeatureAlreadyExis() throws Exception {
        // Given
        Feature fp = new Feature(FEATURE_NEW, true, "description2");
        testedStore.create(fp);
        // When
        Set<String> rights = new HashSet<String>(Arrays.asList(new String[] {ROLE_USER}));
        Feature fp2 = new Feature(FEATURE_NEW, true, G1, "description3", rights);
        testedStore.create(fp2);
        // Then, expected exception
    }

    /**
     * Test that disable is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testDeleteFeature() throws Exception {
        // Given
        assertFf4j.assertThatFeatureExist(F1);
        Feature tmpf1 = testedStore.read(F1);
        int initialNumber = testedStore.readAll().size();
        // When
        testedStore.delete(F1);
        // Then
        assertFf4j.assertThatStoreHasSize(initialNumber - 1);
        assertFf4j.assertThatFeatureDoesNotExist(F1);
        // End, Reinit initial state
        testedStore.create(tmpf1);
        assertFf4j.assertThatFeatureExist(F1);
    }

    /**
     * Test that disable unknown feature through exception .
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testDeteleFeatureDoesnotExist() throws Exception {
        // When
        testedStore.delete(F_DOESNOTEXIST);
        // Then , expected error
    }

    /**
     * Test that grant on unknwow role is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testGrantRoleToFeatureRoleDoesNotExist() throws Exception {
        // When
        testedStore.grantRoleOnFeature(F1, ROLE_NEW);
        // Then
        assertFf4j.assertThatFeatureHasRole(F1, ROLE_NEW);
    }

    /**
     * Test that grant on unknwow feature through exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testGrantRoleToFeatureFeatureDoesNotExist() throws Exception {
        // When
        testedStore.grantRoleOnFeature(F_DOESNOTEXIST, ROLE_USER);
        // Then, expected failure
    }

    /**
     * Test that remove role on feature is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testDeleteRoleToFeature() throws Exception {
        // When
        testedStore.removeRoleFromFeature(F1, ROLE_USER);
        // Then
        assertFf4j.assertThatFeatureHasNotRole(F1, ROLE_USER);
    }

    /**
     * Test that remove role on unknow feature through exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testDeleteRoleFeatureDoesNotExit() {
        // When
        testedStore.removeRoleFromFeature(F_DOESNOTEXIST, ROLE_USER);
        // Then, expected to fail
    }

    /**
     * Test update feature, core DATA
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testUpdateFeatureCoreData() throws Exception {
        // Parameters
        String newDescription = "new-description";
        FlipStrategy newStrategy = new PonderationFlipStrategy(0.12);
        // Given
        assertFf4j.assertThatFeatureExist(F1);
        Assert.assertFalse(newDescription.equals(testedStore.read(F1).getDescription()));
        // When
        Feature fpBis = testedStore.read(F1);
        fpBis.setDescription(newDescription);
        fpBis.setFlippingStrategy(newStrategy);
        testedStore.update(fpBis);
        // Then
        Feature updatedFeature = testedStore.read(F1);
        Assert.assertTrue(newDescription.equals(updatedFeature.getDescription()));
        Assert.assertEquals(newStrategy.toString(), updatedFeature.getFlippingStrategy().toString());
    }

    /**
     * Test update feature, more authorizations.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testUpdateFeatureMoreAutorisation() throws Exception {
        // Parameters
        Set<String> rights2 = new HashSet<String>(Arrays.asList(new String[] {ROLE_USER,ROLE_ADMIN}));
        // Given
        assertFf4j.assertThatFeatureExist(F1);
        assertFf4j.assertThatFeatureHasNotRole(F1, ROLE_ADMIN);
        // When
        Feature fpBis = testedStore.read(F1);
        fpBis.setAuthorizations(rights2);
        testedStore.update(fpBis);
        // Then
        assertFf4j.assertThatFeatureHasRole(F1, ROLE_USER);
        assertFf4j.assertThatFeatureHasRole(F1, ROLE_ADMIN);
    }

    /**
     * Test update feature, less authorizations.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testUpdateFlipLessAutorisation() {
        Feature fpBis = new Feature(F1, false, null);
        testedStore.update(fpBis);
        assertFf4j.assertThatFeatureHasNotRole(F1, ROLE_ADMIN);
    }

    /**
     * Test update feature, more authorizations, not feature
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testUpdateFlipMoreAutorisationNotExist() {
        Set<String> rights2 = new HashSet<String>(Arrays.asList(new String[] {ROLE_USER,ROLE_NEW}));
        Feature fpBis = new Feature(F1, false, G1, "desci2", rights2);
        testedStore.update(fpBis);
        assertFf4j.assertThatFeatureHasRole(F1, ROLE_USER);
        assertFf4j.assertThatFeatureHasRole(F1, ROLE_NEW);
    }

    /**
     * TDD.
     */
    @Test
    public void testExistGroup() {
        Assert.assertTrue(testedStore.existGroup(G1));
        Assert.assertFalse(testedStore.existGroup(G_DOESNOTEXIST));
    }

    /**
     * TDD.
     */
    @Test
    public void testEnableGroup() {
        // Given
        assertFf4j.assertThatFeatureIsDisabled(F2);
        // When
        testedStore.enableGroup(G0);
        // Then
        assertFf4j.assertThatFeatureIsEnabled(F2);
        // Reinit
        testedStore.disable(F2);
    }

    /**
     * TDD.
     */
    @Test(expected = GroupNotFoundException.class)
    public void testEnableGroupDoesNotExist() {
        testedStore.enableGroup(G_DOESNOTEXIST);
    }

    /**
     * TDD.
     */
    @Test
    public void testDisableGroup() {
        // Given
        assertFf4j.assertThatFeatureIsEnabled(F4);
        // When
        testedStore.disableGroup(G1);
        // Then
        assertFf4j.assertThatFeatureIsDisabled(F4);
        // Cancel modifications
        testedStore.enable(F4);
        assertFf4j.assertThatFeatureIsEnabled(F4);
    }

    /**
     * TDD.
     */
    @Test(expected = GroupNotFoundException.class)
    public void testDisableGroupDoesNotExist() {
        testedStore.disableGroup(G_DOESNOTEXIST);
    }

    /**
     * TDD.
     */
    @Test
    public void testReadGroup() {
        Map<String, Feature> group = testedStore.readGroup(G1);
        Assert.assertEquals(2, group.size());
        Assert.assertTrue(group.containsKey(F3));
        Assert.assertTrue(group.containsKey(F4));
    }

    /**
     * TDD.
     */
    @Test(expected = GroupNotFoundException.class)
    public void testReadGroupDoesnotExist() {
        testedStore.readGroup(G_DOESNOTEXIST);
    }

    /**
     * TDD.
     */
    @Test
    public void testAddToGroup() {
        // Given
        assertFf4j.assertThatGroupHasSize(1, G0);
        // When
        testedStore.addToGroup(F1, G0);
        // Then
        assertFf4j.assertThatGroupHasSize(2, G0);
        // End, Return to initial state
        testedStore.removeFromGroup(F1, G0);
        assertFf4j.assertThatGroupHasSize(1, G0);
    }

    /**
     * TDD.
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testAddToGroupFeatureDoeNotExist() {
        testedStore.addToGroup(F_DOESNOTEXIST, G0);
    }

    /**
     * TDD.
     */
    @Test
    public void testRemoveFromGroup() {
        // Given
        assertFf4j.assertThatGroupHasSize(2, G1);
        // When
        testedStore.removeFromGroup(F3, G1);
        // Then
        assertFf4j.assertThatGroupHasSize(1, G1);
        // End, Return to initial state
        testedStore.addToGroup(F3, G1);
        assertFf4j.assertThatGroupHasSize(2, G1);
    }

    /**
     * TDD.
     */
    @Test(expected = GroupNotFoundException.class)
    public void testRemoveLastFeatureOfGroupDeleteGroup() {
        // Given
        assertFf4j.assertThatGroupHasSize(1, G0);
        // When
        testedStore.removeFromGroup(F2, G0);
        // Then
        testedStore.readGroup(G0);
    }

    /**
     * TDD.
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testRemoveFromGroupFeatureDoeNotExist() {
        testedStore.removeFromGroup(F_DOESNOTEXIST, G0);
    }

    /**
     * TDD.
     */
    @Test(expected = GroupNotFoundException.class)
    public void testRemoveFromGroupDoesNotExist() {
        testedStore.removeFromGroup(F1, G_DOESNOTEXIST);
    }

    /**
     * TDD.
     */
    @Test
    public void testRemoveFromGroupIfNotInGroup() {

        testedStore.removeFromGroup(F1, G0);
    }

    /**
     * TDD.
     */
    @Test
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
