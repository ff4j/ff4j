package org.ff4j.test;

/*
 * #%L
 * ff4j-core
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
import java.util.Map;
import java.util.Set;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.strategy.PonderationFlipStrategy;
import org.junit.Before;
import org.junit.Test;

/**
 * For different store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class AbstractStoreTest {

    /** Initial feature number. */
    private static final int EXPECTED_FEATURES_NUMBERS = 5;

    /** Feature Name. */
    private static final String FEATURE_FIRST = "first";

    /** Feature Name. */
    private static final String FEATURE_DUMMY = "dummy";

    /** Feature Name. */
    private static final String FEATURE_NEW = "new";

    /** Feature Name. */
    private static final String FEATURE_FORTH = "forth";

    /** Feature Name. */
    private static final String ROLE_USER = "ROLE_USER";

    /** Constants for testing. */
    private static final String TESTING_GROUP = "GRP1";

    /** Tested Store. */
    protected FeatureStore testedStore;

    /** Initialize */
    protected FF4j ff4j = null;

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
        assertFf4j.assertFeatureNumber(EXPECTED_FEATURES_NUMBERS);
        assertFf4j.assertFlipped(FEATURE_FIRST);
    }

    /**
     * Test that features have been correctle stored.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testReadllFeatures() {
        assertFf4j.assertFeatureNumber(EXPECTED_FEATURES_NUMBERS);
        Map<String, Feature> features = testedStore.readAll();
        features.containsKey(FEATURE_FIRST);
        features.containsKey(FEATURE_FORTH);
    }

    /**
     * Test that features have been correctle stored.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testReadFullFeature() {
        Feature f = testedStore.read(FEATURE_FORTH);
        Assert.assertEquals(f.getUid(), FEATURE_FORTH);
        Assert.assertTrue(f.getDescription() != null && !"".equals(f.getDescription()));
        Assert.assertTrue(f.getAuthorizations() != null && !f.getAuthorizations().isEmpty());
        assertFf4j.assertHasRole(FEATURE_FORTH, "X");
        assertFf4j.assertInGroup(FEATURE_FORTH, TESTING_GROUP);
    }

    /**
     * Test that working with unknown feature throw exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testFlipWithInvalidNameNotFoundException() {
        ff4j.isFlipped("this_featureName_does_not_exist");
    }

    /**
     * Test that enable unknown feature throw exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testEnableNotFoundException() {
        ff4j.enable(FEATURE_DUMMY);
    }

    /**
     * Test that disable unknown feature throw exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testDisableNotFoundException() {
        ff4j.disable(FEATURE_DUMMY);
    }

    /**
     * Test that enable is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testEnableFeature() {
        ff4j.enable(FEATURE_FIRST);
        assertFf4j.assertFlipped(FEATURE_FIRST);
    }

    /**
     * Test that disable is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testDisableFeature() {
        ff4j.disable(FEATURE_FIRST);
        assertFf4j.assertNotFlipped(FEATURE_FIRST);
    }

    /**
     * Test that addFeature is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testAddFeature() throws Exception {
        Set<String> rights = new HashSet<String>(Arrays.asList(new String[] {ROLE_USER}));
        Feature fp = new Feature(FEATURE_NEW, true, "description", TESTING_GROUP, rights);
        assertFf4j.assertNotExist(FEATURE_NEW);

        testedStore.create(fp);
        assertFf4j.assertFeatureNumber(EXPECTED_FEATURES_NUMBERS + 1);
        assertFf4j.assertExist(FEATURE_NEW);
        assertFf4j.assertInGroup(FEATURE_NEW, TESTING_GROUP);
    }

    /**
     * Test that creating feature already exist through exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureAlreadyExistException.class)
    public void testAddFeatureAlreadyExis() throws Exception {
        // Create Once
        Feature fp = new Feature(FEATURE_NEW, true, "description2");
        testedStore.create(fp);

        Set<String> rights = new HashSet<String>(Arrays.asList(new String[] {ROLE_USER}));
        Feature fp2 = new Feature(FEATURE_NEW, true, TESTING_GROUP, "description3", rights);
        testedStore.create(fp2);
    }

    /**
     * Test that disable is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testDeleteFeature() throws Exception {
        Set<String> rights = new HashSet<String>(Arrays.asList(new String[] {ROLE_USER}));
        Feature fp2 = new Feature("TO_BE_DELETED", true, TESTING_GROUP, "description4", rights);
        int current = testedStore.readAll().size();
        // Create Once
        testedStore.create(fp2);
        assertFf4j.assertFeatureNumber(current + 1);

        testedStore.delete(fp2.getUid());
        assertFf4j.assertFeatureNumber(current);
    }

    /**
     * Test that disable unknown feature through exception .
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testDeteleFeatureDoesnotExistReturnError() throws Exception {
        testedStore.delete("does-not-exist");
    }

    /**
     * Test that grant on unknwow role is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testGrantRoleToFeatureRoleDoesNotExist() throws Exception {
        testedStore.grantRoleOnFeature(FEATURE_FIRST, "role-does-not-exit1");
        assertFf4j.assertHasRole(FEATURE_FIRST, "role-does-not-exit1");
    }

    /**
     * Test that grant on unknwow feature through exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testGrantRoleToFeatureFeatureDoesNotExist() throws Exception {
        testedStore.grantRoleOnFeature("blablabla", "role-does-not-exit");
    }

    /**
     * Test that remove role on feature is OK.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testDeleteRoleToFeature() throws Exception {
        testedStore.removeRoleFromFeature(FEATURE_FIRST, ROLE_USER);
        assertFf4j.assertDoesntHaveRole(FEATURE_FIRST, ROLE_USER);
    }

    /**
     * Test that remove role on unknow feature through exception.
     * 
     * @throws Exception
     *             error during test
     */
    @Test(expected = FeatureNotFoundException.class)
    public void testDeleteRoleFeatureDoesNotExit() {
        testedStore.removeRoleFromFeature("blabla", ROLE_USER);
    }

    /**
     * Test update feature, core DATA
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testUpdateFeatureCoreData() throws Exception {
        Feature fpBis = new Feature(FEATURE_FIRST, false, "desca2");
        fpBis.setFlippingStrategy(new PonderationFlipStrategy(0.12));
        testedStore.update(fpBis);
        assertFf4j.assertNotFlipped(FEATURE_FIRST);
        Assert.assertEquals("desca2", testedStore.read(FEATURE_FIRST).getDescription());
    }

    /**
     * Test update feature, more authorizations.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testUpdateFeatureMoreAutorisation() throws Exception {
        Set<String> rights2 = new HashSet<String>(Arrays.asList(new String[] {ROLE_USER,"X"}));
        Feature fpBis = new Feature(FEATURE_FIRST, false, TESTING_GROUP, "desco2", rights2);
        testedStore.update(fpBis);
        assertFf4j.assertHasRole(FEATURE_FIRST, ROLE_USER);
        assertFf4j.assertHasRole(FEATURE_FIRST, "X");
    }

    /**
     * Test update feature, less authorizations.
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testUpdateFlipLessAutorisation() {
        Feature fpBis = new Feature(FEATURE_FIRST, false, null);
        testedStore.update(fpBis);
        assertFf4j.assertDoesntHaveRole(FEATURE_FIRST, "x");
    }

    /**
     * Test update feature, more authorizations, not feature
     * 
     * @throws Exception
     *             error during test
     */
    @Test
    public void testUpdateFlipMoreAutorisationNotExistReturnError() {
        Set<String> rights2 = new HashSet<String>(Arrays.asList(new String[] {ROLE_USER,"ROLE_ADMIN"}));
        Feature fpBis = new Feature(FEATURE_FIRST, false, TESTING_GROUP, "desci2", rights2);
        testedStore.update(fpBis);
        assertFf4j.assertHasRole(FEATURE_FIRST, ROLE_USER);
        assertFf4j.assertHasRole(FEATURE_FIRST, "ROLE_ADMIN");
    }
}
