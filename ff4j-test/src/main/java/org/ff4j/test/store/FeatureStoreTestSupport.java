package org.ff4j.test.store;

import static org.ff4j.test.TestsFf4jConstants.EXPECTED_FEATURES_NUMBERS;
import static org.ff4j.test.TestsFf4jConstants.F1;
import static org.ff4j.test.TestsFf4jConstants.F2;
import static org.ff4j.test.TestsFf4jConstants.F3;
import static org.ff4j.test.TestsFf4jConstants.F4;
import static org.ff4j.test.TestsFf4jConstants.FEATURE_NEW;
import static org.ff4j.test.TestsFf4jConstants.F_DOESNOTEXIST;
import static org.ff4j.test.TestsFf4jConstants.G0;
import static org.ff4j.test.TestsFf4jConstants.G1;
import static org.ff4j.test.TestsFf4jConstants.G_DOESNOTEXIST;
import static org.ff4j.test.TestsFf4jConstants.ROLE_ADMIN;
import static org.ff4j.test.TestsFf4jConstants.ROLE_NEW;
import static org.ff4j.test.TestsFf4jConstants.ROLE_USER;
import static org.ff4j.test.TestsFf4jConstants.TEST_FEATURES_FILE;

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

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.property.PropertyInt;
import org.ff4j.property.PropertyString;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.PonderationStrategy;
import org.ff4j.test.AssertFf4j;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * For different store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class FeatureStoreTestSupport {

	public static final String GOLOGOLO = "GOLOGOLO";
	public static final String ROLE_XYZ = "ROLE_XYZ";
	public static final String PPSTRING = "ppstring";
	public static final String DIGIT_VALUE = "digitValue";
	public static final String REGION_IDENTIFIER = "regionIdentifier";

	/** Initialize */
	protected FF4j ff4j = null;

	/** Tested Store. */
	protected FeatureStore testedStore;

	/** Test Values */
	protected AssertFf4j assertFf4j;

	/** Default InMemoryStore for test purposes. */
	protected FeatureStore defaultStore = new InMemoryFeatureStore(TEST_FEATURES_FILE);

	/** {@inheritDoc} */
	@Before
	public void setUp() throws Exception {
		ff4j = new FF4j();
		ff4j.setFeatureStore(initStore());
		testedStore = ff4j.getFeatureStore();
		assertFf4j = new AssertFf4j(ff4j);
		assertFf4j.setPause(enablePause());
	}

	/**
	 * Any store test will declare its store through this callback.
	 * 
	 * @return working feature store
	 * @throws Exception
	 *             error during building feature store
	 */
	protected abstract FeatureStore initStore();

	protected int enablePause() {
		return 0;
	}

	/**
	 * TDD.
	 */
	@Test
	public void testStoreHasBeenInitialized() {
		// Given
		assertFf4j.assertThatStoreHasSize(EXPECTED_FEATURES_NUMBERS);
		assertFf4j.assertThatFeatureFlipped(F1);
	}

	/**
	 * TDD.
	 */
	@Test
	public void testReadAllFeatures() {
		// Given
		assertFf4j.assertThatFeatureExist(F4);
		assertFf4j.assertThatStoreHasSize(EXPECTED_FEATURES_NUMBERS);
		// When
		Map<String, Feature> features = testedStore.readAll();
		// Then
		Assert.assertEquals(EXPECTED_FEATURES_NUMBERS, features.size());
		// Then testing whole structure
		Feature f = features.get(F4);
		Assert.assertEquals(F4 + " does not exist", f.getUid(), F4);
		Assert.assertTrue("no description", f.getDescription() != null && !"".equals(f.getDescription()));
		Assert.assertTrue("no authorizations", f.getPermissions() != null && !f.getPermissions().isEmpty());
		assertFf4j.assertThatFeatureHasRole(F4, ROLE_ADMIN);
		assertFf4j.assertThatFeatureIsInGroup(F4, G1);
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testReadNull() {
		// Given
		// When
		testedStore.read(null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testReadEmpty() {
		// Given
		// When
		testedStore.read("");
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = FeatureNotFoundException.class)
	public void testReadNotExist() {
		// Given
		// When
		testedStore.read("I-DONT-EXIST");
		// Then, expected error...
	}

	/**
	 * TDD.
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
		Assert.assertTrue(f.getPermissions() != null && !f.getPermissions().isEmpty());
		assertFf4j.assertThatFeatureHasRole(F4, ROLE_ADMIN);
		assertFf4j.assertThatFeatureIsInGroup(F4, G1);
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testEnableNull() {
		// Given
		// When
		testedStore.enable(null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testEnableEmpty() {
		// Given
		// When
		testedStore.enable("");
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = FeatureNotFoundException.class)
	public void testEnableFeatureDoesNotExist() {
		// Given
		assertFf4j.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
		// When
		testedStore.enable(F_DOESNOTEXIST);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testDisableNull() {
		// Given
		// When
		testedStore.disable(null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testDisableEmpty() {
		// Given
		// When
		testedStore.disable("");
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = FeatureNotFoundException.class)
	public void testDisableFeatureDoesNotExist() {
		// Given
		assertFf4j.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
		// When
		testedStore.disable(F_DOESNOTEXIST);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test
	public void testEnableFeature() {
		// Given
		assertFf4j.assertThatFeatureExist(F1);
		// When
		testedStore.enable(F1);
		// Then
		assertFf4j.assertThatFeatureIsEnabled(F1);
	}

	/**
	 * TDD.
	 */
	@Test
	public void testDisableFeature() {
		// Given
		assertFf4j.assertThatFeatureExist(F1);
		// When
		testedStore.disable(F1);
		// Then
		assertFf4j.assertThatFeatureIsDisabled(F1);
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testCreateNull() throws Exception {
		// Given
		// When
		testedStore.create(null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test
	public void testAddFeature() throws Exception {
		// Given
		assertFf4j.assertThatFeatureDoesNotExist(FEATURE_NEW);
		// When
		Set<String> rights = new HashSet<String>(Arrays.asList(new String[] { ROLE_USER }));
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
	 * TDD.
	 */
	@Test(expected = FeatureAlreadyExistException.class)
	public void testAddFeatureAlreadyExis() throws Exception {
		// Given
		assertFf4j.assertThatFeatureDoesNotExist(GOLOGOLO);
		// When (first creation)
		Feature fp = new Feature(GOLOGOLO, true, "description2");
		testedStore.create(fp);
		// Then (first creation)
		assertFf4j.assertThatFeatureExist(GOLOGOLO);
		// When (second creation)
		Set<String> rights = new HashSet<String>(Arrays.asList(new String[] { ROLE_USER }));
		Feature fp2 = new Feature(GOLOGOLO, true, G1, "description3", rights);
		testedStore.create(fp2);
		// Then, expected exception
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testDeleteNull() throws Exception {
		// Given
		// When
		testedStore.delete(null);
		// Then, expected error...
	}

	/**
	 * TDD.
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
	 * TDD.
	 */
	@Test(expected = FeatureNotFoundException.class)
	public void testDeteleFeatureDoesnotExist() throws Exception {
		// Given
		assertFf4j.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
		// When
		testedStore.delete(F_DOESNOTEXIST);
		// Then , expected error
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGrantRoleNullFeature() throws Exception {
		// Given
		// When
		testedStore.grantRoleOnFeature(null, ROLE_ADMIN);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGrantRoleEmptyFeature() throws Exception {
		// Given
		// When
		testedStore.grantRoleOnFeature("", ROLE_ADMIN);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGrantRoleNullRole() throws Exception {
		// Given
		// When
		testedStore.grantRoleOnFeature(F1, null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGrantRoleEmptyRole() throws Exception {
		// Given
		// When
		testedStore.grantRoleOnFeature(F1, "");
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test
	public void testGrantRoleToFeatureRoleDoesNotExist() throws Exception {
		// Given
		assertFf4j.assertThatFeatureExist(F1);
		assertFf4j.assertThatFeatureHasNotRole(F1, ROLE_XYZ);
		// When
		testedStore.grantRoleOnFeature(F1, ROLE_XYZ);
		// Then
		assertFf4j.assertThatFeatureHasRole(F1, ROLE_XYZ);
	}

	/**
	 * TDD.
	 */
	@Test(expected = FeatureNotFoundException.class)
	public void testGrantRoleToFeatureFeatureDoesNotExist() throws Exception {
		// Given
		assertFf4j.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
		// When
		testedStore.grantRoleOnFeature(F_DOESNOTEXIST, ROLE_USER);
		// Then, expected failure
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testRemoveRoleNullFeature() throws Exception {
		// Given
		// When
		testedStore.removeRoleFromFeature(null, ROLE_ADMIN);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testRemoveRoleEmptyFeature() throws Exception {
		// Given
		// When
		testedStore.removeRoleFromFeature("", ROLE_ADMIN);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testRemoveRoleNullRole() throws Exception {
		// Given
		// When
		testedStore.removeRoleFromFeature(F1, null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testRemoveRoleEmptyRole() throws Exception {
		// Given
		// When
		testedStore.removeRoleFromFeature(F1, "");
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test
	public void testDeleteRoleToFeature() throws Exception {
		// Given
		assertFf4j.assertThatFeatureExist(F1);
		assertFf4j.assertThatFeatureHasRole(F1, ROLE_USER);
		// When
		testedStore.removeRoleFromFeature(F1, ROLE_USER);
		// Then
		assertFf4j.assertThatFeatureHasNotRole(F1, ROLE_USER);
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testUpdateNull() throws Exception {
		// Given
		// When
		testedStore.update(null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = FeatureNotFoundException.class)
	public void testFeatureDoesNotExit() {
		// Given
		assertFf4j.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
		// When
		testedStore.update(new Feature(F_DOESNOTEXIST));
		// Then, expect error
	}

	/**
	 * TDD.
	 */
	@Test(expected = FeatureNotFoundException.class)
	public void testDeleteRoleFeatureDoesNotExit() {
		// Given
		assertFf4j.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
		// When
		testedStore.removeRoleFromFeature(F_DOESNOTEXIST, ROLE_USER);
		// Then, expected to fail
	}

	/**
	 * TDD.
	 */
	@Test
	public void testUpdateFeatureCoreData() {
		// Parameters
		String newDescription = "new-description";
		FlippingStrategy newStrategy = new PonderationStrategy(0.12);
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
		Assert.assertNotNull(updatedFeature.getFlippingStrategy());
		Assert.assertEquals(newStrategy.toString(), updatedFeature.getFlippingStrategy().toString());
	}

	/**
	 * TDD.
	 */
	@Test
	public void testUpdateFeatureMoreAutorisation() {
		// Parameters
		Set<String> rights2 = new HashSet<String>(Arrays.asList(new String[] { ROLE_USER, ROLE_ADMIN }));
		// Given
		assertFf4j.assertThatFeatureExist(F1);
		assertFf4j.assertThatFeatureHasNotRole(F1, ROLE_ADMIN);
		// When
		Feature fpBis = testedStore.read(F1);
		fpBis.setPermissions(rights2);
		testedStore.update(fpBis);
		// Then
		assertFf4j.assertThatFeatureHasRole(F1, ROLE_USER);
		assertFf4j.assertThatFeatureHasRole(F1, ROLE_ADMIN);
	}

	/**
	 * TDD.
	 */
	@Test
	public void testUpdateFlipLessAutorisation() {
		// Given
		assertFf4j.assertThatFeatureExist(F2);
		assertFf4j.assertThatFeatureHasRole(F2, ROLE_USER);
		// When
		testedStore.update(new Feature(F2, false, null));
		// Then
		assertFf4j.assertThatFeatureHasNotRole(F2, ROLE_USER);
	}

	/**
	 * TDD.
	 */
	@Test
	public void testUpdateFlipMoreAutorisationNotExist() {
		// Given
		assertFf4j.assertThatFeatureHasNotRole(F1, ROLE_NEW);
		Set<String> rights2 = new HashSet<String>(Arrays.asList(new String[] { ROLE_USER, ROLE_NEW }));
		Feature fpBis = new Feature(F1, false, G1, "desci2", rights2);
		// When
		testedStore.update(fpBis);
		// Then
		assertFf4j.assertThatFeatureHasRole(F1, ROLE_USER);
		assertFf4j.assertThatFeatureHasRole(F1, ROLE_NEW);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testExistNull() {
		ff4j.getFeatureStore().exist(null);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testExistEmpty() {
		ff4j.getFeatureStore().exist("");
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testExistGroupNull() throws Exception {
		// Given
		// When
		testedStore.existGroup(null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testExistGroupEmpty() throws Exception {
		// Given
		// When
		testedStore.existGroup("");
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test
	public void testExistGroup() {
		// Given
		assertFf4j.assertThatGroupExist(G1);
		assertFf4j.assertThatGroupDoesNotExist(G_DOESNOTEXIST);
		// Then
		Assert.assertTrue(testedStore.existGroup(G1));
		Assert.assertFalse(testedStore.existGroup(G_DOESNOTEXIST));
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testEnableGroupNull() throws Exception {
		// Given
		// When
		testedStore.enableGroup(null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testEnableGroupEmpty() throws Exception {
		// Given
		// When
		testedStore.enableGroup("");
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test
	public void testEnableGroup() {
		// Given
		testedStore.disable(F2);
		testedStore.addToGroup(F2, G0);
		assertFf4j.assertThatFeatureIsDisabled(F2);
		assertFf4j.assertThatFeatureIsInGroup(F2, G0);
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
		// Given
		assertFf4j.assertThatGroupDoesNotExist(G_DOESNOTEXIST);
		// When
		testedStore.enableGroup(G_DOESNOTEXIST);
		// Then, expected error
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testDisableGroupNull() throws Exception {
		// Given
		// When
		testedStore.disableGroup(null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testDisableGroupEmpty() throws Exception {
		// Given
		// When
		testedStore.disableGroup("");
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test
	public void testDisableGroup() {
		// Given
		assertFf4j.assertThatFeatureIsEnabled(F4);
		assertFf4j.assertThatFeatureIsInGroup(F4, G1);
		// When
		testedStore.disableGroup(G1);
		// Then
		assertFf4j.assertThatFeatureIsDisabled(F4);
		// Rollback modifications
		testedStore.enable(F4);
		assertFf4j.assertThatFeatureIsEnabled(F4);
	}

	/**
	 * TDD.
	 */
	@Test(expected = GroupNotFoundException.class)
	public void testDisableGroupDoesNotExist() {
		// Given
		assertFf4j.assertThatGroupDoesNotExist(G_DOESNOTEXIST);
		// When
		testedStore.disableGroup(G_DOESNOTEXIST);
		// Then, expected error
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testReadGroupNull() throws Exception {
		// Given
		// When
		testedStore.readGroup(null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testReadGroupEmpty() throws Exception {
		// Given
		// When
		testedStore.readGroup("");
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test
	public void testReadGroup() {
		// Given
		assertFf4j.assertThatGroupExist(G1);
		assertFf4j.assertThatFeatureExist(F3);
		assertFf4j.assertThatFeatureExist(F4);
		testedStore.addToGroup(F3, G1);
		testedStore.addToGroup(F4, G1);
		assertFf4j.assertThatFeatureIsInGroup(F3, G1);
		assertFf4j.assertThatFeatureIsInGroup(F4, G1);
		// When
		Map<String, Feature> group = testedStore.readGroup(G1);
		// Then
		Assert.assertEquals(2, group.size());
		Assert.assertTrue(group.containsKey(F3));
		Assert.assertTrue(group.containsKey(F4));
	}

	/**
	 * TDD.
	 */
	@Test(expected = GroupNotFoundException.class)
	public void testReadGroupDoesnotExist() {
		// Given
		assertFf4j.assertThatGroupDoesNotExist(G_DOESNOTEXIST);
		// When
		testedStore.readGroup(G_DOESNOTEXIST);
		// Then, expect error
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testAddToGroupFeatureNull() throws Exception {
		// Given
		// When
		testedStore.addToGroup(null, G0);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testAddToGroupFeatureEmpty() throws Exception {
		// Given
		// When
		testedStore.addToGroup("", G0);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testAddToGroupNull() throws Exception {
		// Given
		// When
		testedStore.addToGroup(F1, null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testAddToGroupEmpty() throws Exception {
		// Given
		// When
		testedStore.addToGroup(F1, "");
		// Then, expected error...
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
		// Given
		assertFf4j.assertThatGroupDoesNotExist(G_DOESNOTEXIST);
		// When
		testedStore.addToGroup(F_DOESNOTEXIST, G0);
		// Then, expected error
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testRemoveToGroupFeatureNull() throws Exception {
		// Given
		// When
		testedStore.removeFromGroup(null, G0);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testRemoveToGroupFeatureEmpty() throws Exception {
		// Given
		// When
		testedStore.removeFromGroup("", G0);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testRemoveToGroupNull() throws Exception {
		// Given
		// When
		testedStore.removeFromGroup(F1, null);
		// Then, expected error...
	}

	/**
	 * TDD.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testRemoveToGroupEmpty() throws Exception {
		// Given
		// When
		testedStore.removeFromGroup(F1, "");
		// Then, expected error...
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
		assertFf4j.assertThatGroupExist(G0);
		assertFf4j.assertThatGroupHasSize(1, G0);
		// When
		testedStore.removeFromGroup(F2, G0);
		// Then
		
		assertFf4j.assertThatGroupDoesNotExist(G0);
		// Expected error
		testedStore.readGroup(G0);
	}

	/**
	 * TDD.
	 */
	@Test(expected = FeatureNotFoundException.class)
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
	@Test(expected = GroupNotFoundException.class)
	public void testRemoveFromGroupDoesNotExist() {
		// Given
		assertFf4j.assertThatFeatureExist(F1);
		assertFf4j.assertThatGroupDoesNotExist(G_DOESNOTEXIST);
		// When
		testedStore.removeFromGroup(F1, G_DOESNOTEXIST);
		// Then, expected error
	}

	/**
	 * TDD.
	 */
	@Test
	public void testRemoveFromGroupIfNotInGroup() {
		// Given
		assertFf4j.assertThatFeatureExist(F1);
		assertFf4j.assertThatGroupExist(G1);
		// When
		testedStore.removeFromGroup(F1, G1);
		// Then : nothing special
	}

	/**
	 * TDD.
	 */
	@Test
	public void testReadAllGroup() {
		// Reinit
		testedStore.addToGroup(F2, G0);
		// Given
		assertFf4j.assertThatStoreHasNumberOfGroups(2);
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
	public void testUpdateEditFlippingStrategy() {
		// Given
		assertFf4j.assertThatFeatureExist(F3);
		// When
		Feature myFeature = ff4j.getFeatureStore().read(F3);
		myFeature.setFlippingStrategy(new PonderationStrategy(0.1));
		testedStore.update(myFeature);
		// Then
		assertFf4j.assertThatFeatureHasFlippingStrategy(F3);
	}

	/**
	 * TDD.
	 */
	@Test
	public void testUpdateRemoveFlippingStrategy() {
		// Given
		assertFf4j.assertThatFeatureExist(F3);
		Feature myFeature = ff4j.getFeatureStore().read(F3);
		myFeature.setFlippingStrategy(new PonderationStrategy(0.1));
		testedStore.update(myFeature);
		assertFf4j.assertThatFeatureHasFlippingStrategy(F3);
		// When
		Feature myFeature2 = ff4j.getFeatureStore().read(F3);
		myFeature2.setFlippingStrategy(null);
		testedStore.update(myFeature2);
		// Then
		assertFf4j.assertThatFeatureDoesNotHaveFlippingStrategy(F3);
	}

	/**
	 * TDD.
	 */
	@Test
	public void testUpdateAddFlippingStrategy() {
		// Given
		assertFf4j.assertThatFeatureExist(F2);
		assertFf4j.assertThatFeatureDoesNotHaveFlippingStrategy(F2);
		// When
		Feature myFeature = ff4j.getFeatureStore().read(F2);
		myFeature.setFlippingStrategy(new PonderationStrategy(0.1));
		testedStore.update(myFeature);
		// Then
		assertFf4j.assertThatFeatureHasFlippingStrategy(F2);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testDonotUpdateNullFeature() {
		testedStore.update(null);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testDonotDeleteNull() {
		testedStore.delete(null);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testDonotDeleteEmpty() {
		testedStore.delete("");
	}

	@Test
	public void testClear() {
		// Given
		Assert.assertNotNull(testedStore);
		Map<String, Feature> before = testedStore.readAll();
		Assert.assertFalse(before.isEmpty());
		// When
		testedStore.clear();
		// Then
		Assert.assertTrue(testedStore.readAll().isEmpty());
		/// Reinit
		for (Map.Entry<String, Feature> pName : before.entrySet()) {
			testedStore.create(pName.getValue());
		}
	}

	/**
	 * TDD.
	 */
	@Test
	public void testUpdateAddProperty() {
		// Given
		assertFf4j.assertThatFeatureExist(F2);
		assertFf4j.assertThatFeatureHasNotProperty(F2, "p1");
		// When
		Feature myFeature = ff4j.getFeatureStore().read(F2);
		PropertyString p1 = new PropertyString("p1", "v1", Util.set("v1", "v2"));
		myFeature.getCustomProperties().put(p1.getName(), p1);
		testedStore.update(myFeature);
		// Then
		assertFf4j.assertThatFeatureHasProperty(F2, "p1");
	}

	/**
	 * TDD.
	 */
	@Test
	public void testUpdateRemoveProperty() {
		// Given
		assertFf4j.assertThatFeatureExist(F1);
		// assertFf4j.assertThatFeatureHasProperty(F1, "ppint");
		// When
		Feature myFeature = ff4j.getFeatureStore().read(F1);
		myFeature.getCustomProperties().remove("ppint");
		testedStore.update(myFeature);
		// Then
		assertFf4j.assertThatFeatureHasNotProperty(F1, "p1");
	}

	/**
	 * TDD.
	 */
	@Test
	public void testUpdateEditPropertyValue() {
		// Given
		assertFf4j.assertThatFeatureExist(F1);
		Feature myFeature = ff4j.getFeatureStore().read(F1);
		if (myFeature.getCustomProperties().isEmpty()) {
			PropertyString p1 = new PropertyString(PPSTRING);
			p1.setValue("hello");
			myFeature.getCustomProperties().put(p1.getName(), p1);
			testedStore.update(myFeature);
		}
		assertFf4j.assertThatFeatureHasProperty(F1, PPSTRING);
		Assert.assertEquals("hello",
				ff4j.getFeatureStore().read(F1)//
						.getCustomProperties().get(PPSTRING)//
						.asString());
		// When
		myFeature = ff4j.getFeatureStore().read(F1);
		PropertyString p1 = new PropertyString(PPSTRING, "goodbye");
		myFeature.getCustomProperties().put(p1.getName(), p1);
		testedStore.update(myFeature);

		// Then
		Assert.assertEquals("goodbye",
				ff4j.getFeatureStore().read(F1)//
						.getCustomProperties().get(PPSTRING)//
						.asString());
	}

	/**
	 * TDD.
	 */
	@Test
	@SuppressWarnings("unchecked")
	public void testUpdateEditPropertyAddFixedValues() {
		// Given
		assertFf4j.assertThatFeatureExist(F1);
		Feature myFeature = ff4j.getFeatureStore().read(F1);
		myFeature.addProperty(new PropertyInt(DIGIT_VALUE, 2, Util.set(0, 1, 2, 3)));
		ff4j.getFeatureStore().update(myFeature);
		assertFf4j.assertThatFeatureHasProperty(F1, DIGIT_VALUE);

		Set<Integer> fixValues = (Set<Integer>) ff4j.getFeatureStore().read(F1)//
				.getCustomProperties().get(DIGIT_VALUE).getFixedValues();
		Assert.assertEquals(4, fixValues.size());

		// When
		myFeature = ff4j.getFeatureStore().read(F1);
		PropertyInt p1 = new PropertyInt(DIGIT_VALUE);
		p1.setFixedValues(Util.set(0, 1, 2, 3, 4));
		p1.setValue(4);
		myFeature.getCustomProperties().put(p1.getName(), p1);
		testedStore.update(myFeature);

		// Then
		Set<Integer> fixValues2 = (Set<Integer>) ff4j.getFeatureStore().read(F1) //
				.getCustomProperties().get(DIGIT_VALUE).getFixedValues();
		Assert.assertEquals(5, fixValues2.size());
	}

	/**
	 * TDD.
	 */
	@Test
	@SuppressWarnings("unchecked")
	public void testUpdateEditPropertyRemoveFixedValues() {
		// Given
		assertFf4j.assertThatFeatureExist(F1);
		Feature myFeature = ff4j.getFeatureStore().read(F1);
		myFeature.addProperty(new PropertyString(REGION_IDENTIFIER, "AMER", Util.set("AMER", "SSSS", "EAST")));
		testedStore.update(myFeature);
		assertFf4j.assertThatFeatureHasProperty(F1, REGION_IDENTIFIER);
		Set<String> fixValues = (Set<String>) ff4j.getFeatureStore().read(F1)//
				.getCustomProperties().get(REGION_IDENTIFIER).getFixedValues();
		Assert.assertEquals(3, fixValues.size());

		// When
		myFeature = ff4j.getFeatureStore().read(F1);
		PropertyString p1 = new PropertyString(REGION_IDENTIFIER);
		p1.setValue("AMER");
		p1.setFixedValues(Util.set("AMER", "SSSS"));
		myFeature.getCustomProperties().put(p1.getName(), p1);
		testedStore.update(myFeature);

		// Then
		Set<Integer> fixValues2 = (Set<Integer>) ff4j.getFeatureStore().read(F1)//
				.getCustomProperties().get(REGION_IDENTIFIER).getFixedValues();
		Assert.assertEquals(2, fixValues2.size());
	}

}
