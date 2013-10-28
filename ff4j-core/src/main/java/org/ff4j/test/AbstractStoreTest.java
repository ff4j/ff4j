package org.ff4j.test;

/*
 * #%L ff4j-test %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use
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
import java.util.Set;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.test.AssertFf4j;
import org.junit.Test;

/**
 * For different.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class AbstractStoreTest extends TestCase {

    /** Tested Store. */
    protected FeatureStore testedStore;

    /** Initialize */
    protected FF4j ff4j = null;

    /** Test Values */
    protected AssertFf4j assertFf4j;

    /** {@inheritDoc} */
    @Override
    protected void setUp() throws Exception {
        super.setUp();
        if (ff4j == null) {
            ff4j = new FF4j();
            ff4j.setStore(initStore());
            testedStore = ff4j.getStore();
            assertFf4j = new AssertFf4j(ff4j);
        }
    }

    /**
     * Any store test will declare its store through this callback.
     * 
     * @return working feature store
     * @throws Exception
     *             error during building feature store
     */
    protected abstract FeatureStore initStore() throws Exception;

    @Test
    public void testStoreHasBeenInitialized() throws Exception {
        assertFf4j.assertFeatureNumber(5);
        assertFf4j.assertFlipped("first");
    }

    @Test
    public void testFlipWithInvalidName_NotFoundException() {
        try {
            ff4j.isFlipped("this_featureName_does_not_exist");
            fail("this_featureName_does_not_exist should not exist");
        } catch (FeatureNotFoundException fue) {
            Assert.assertTrue(fue.getMessage().contains("this_featureName_does_not_exist"));
        }
    }

    @Test
    public void testEnable_NotFoundException() {
        try {
            ff4j.enable("dummy");
            fail();
        } catch (FeatureNotFoundException fue) {
            Assert.assertTrue(fue.getMessage().contains("dummy"));
        }
    }

    @Test
    public void testDisable_NotFoundException() {
        try {
            ff4j.disable("dummy");
            fail();
        } catch (FeatureNotFoundException fue) {
            Assert.assertTrue(fue.getMessage().contains("dummy"));
        }
    }

    @Test
    public void testEnableFeature() {
        ff4j.enable("first");
        assertFf4j.assertFlipped("first");
    }

    @Test
    public void testDisableFeature() {
        ff4j.disable("first");
        assertFf4j.assertNotFlipped("first");
    }

    @Test
    public void testAddFeature() throws Exception {
        Set<String> rights = new HashSet<String>(Arrays.asList(new String[] {"ROLE_USER"}));
        Feature fp = new Feature("new", true, "description", rights);
        testedStore.create(fp);
        assertFf4j.assertFeatureNumber(6);
        assertFf4j.assertExist("new");
    }

    @Test
    public void testAddFeature_AlreadyExis() throws Exception {
        if (!testedStore.exist("new")) {
            Feature fp = new Feature("new", true, "description");
            testedStore.create(fp);
        }

        Set<String> rights = new HashSet<String>(Arrays.asList(new String[] {"ROLE_USER"}));
        Feature fp2 = new Feature("new", true, "description", rights);
        try {
            testedStore.create(fp2);
            fail();
        } catch (FeatureAlreadyExistException fpaee) {
            // OK
        }

        // Overriding, no error, no new creation
        assertFf4j.assertFeatureNumber(6);
        assertFf4j.assertExist("new");
    }

    @Test
    public void testDeleteFeature() throws Exception {
        Set<String> rights = new HashSet<String>(Arrays.asList(new String[] {"ROLE_USER"}));
        Feature fp2 = new Feature("new", true, "description", rights);
        if (!testedStore.exist("new")) {
            testedStore.create(fp2);
            assertFf4j.assertFeatureNumber(6);
        }
        testedStore.delete(fp2.getUid());
        assertFf4j.assertFeatureNumber(5);
    }

    @Test
    public void testDeteleFeature_DoesnotExistReturnError() throws Exception {
        try {
            testedStore.delete("does-not-exist");
            fail();
        } catch (FeatureNotFoundException fnfe) {
            // @Test(expected = FlipPointNotFoundException.class) doesn't work everytime...
        }
    }

    @Test
    public void testAddRoleToFeatureThatDoesNotExistReturnException() throws Exception {
        testedStore.grantRoleOnFeature("first", "role-does-not-exit");
        assertFf4j.assertHasRole("first", "role-does-not-exit");
    }

    @Test
    public void testDeleteRole_ToFeature() throws Exception {
        testedStore.removeRoleFromFeature("first", "ROLE_USER");
        assertFf4j.assertDoesntHaveRole("first", "ROLE_USER");
    }

    @Test
    public void testUpdateFeature_CoreData() throws Exception {
        Feature fpBis = new Feature("first", false, "desc2");
        testedStore.update(fpBis);
        assertFf4j.assertNotFlipped("first");
        Assert.assertEquals("desc2", testedStore.read("first").getDescription());
    }

    @Test
    public void testUpdateFeature_MoreAutorisation() throws Exception {
        Set<String> rights2 = new HashSet<String>(Arrays.asList(new String[] {"ROLE_USER","X"}));
        Feature fpBis = new Feature("first", false, "desc2", rights2);
        testedStore.update(fpBis);
        assertFf4j.assertHasRole("first", "ROLE_USER");
        assertFf4j.assertHasRole("first", "X");
    }

    @Test
    public void testUpdateFlip_LessAutorisation() {
        Feature fpBis = new Feature("first", false, null);
        testedStore.update(fpBis);
        assertFf4j.assertDoesntHaveRole("first", "x");
    }

    @Test
    public void testUpdateFlip_MoreAutorisationNotExist_ReturnError() {
        Set<String> rights2 = new HashSet<String>(Arrays.asList(new String[] {"ROLE_USER","ROLE_ADMIN"}));
        Feature fpBis = new Feature("first", false, "desc2", rights2);
        testedStore.update(fpBis);
        assertFf4j.assertHasRole("first", "ROLE_USER");
        assertFf4j.assertHasRole("first", "ROLE_ADMIN");
    }
}
