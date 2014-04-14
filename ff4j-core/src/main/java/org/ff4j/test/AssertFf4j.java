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

import junit.framework.Assert;

import org.ff4j.FF4j;

/**
 * Give utilities method for tests.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class AssertFf4j {

    /** reference to ff4j context. */
    private final FF4j ff4j;

    /**
     * Initialisation with current ff4j context.
     * 
     * @param ff4j
     *            current ff4k context
     */
    public AssertFf4j(FF4j cff4j) {
        this.ff4j = cff4j;
    }

    /**
     * Check existence of the traget feature
     * 
     * @param featureName
     *            targte featurename
     */
    public final void assertExist(String featureName) {
        Assert.assertTrue(ff4j.exist(featureName));
    }

    /**
     * Check inexistence of the traget feature
     * 
     * @param featureName
     *            targte featurename
     */
    public final void assertNotExist(String featureName) {
        Assert.assertFalse(ff4j.exist(featureName));
    }

    /**
     * Check Feature Flipped
     * 
     * @param featureName
     *            target featureName
     */
    public final void assertFlipped(String featureName) {
        assertExist(featureName);
        Assert.assertTrue("'" + featureName + "' is not flipped where it should", ff4j.isFlipped(featureName));
    }

    /**
     * Check Feature Flipped
     * 
     * @param featureName
     *            target featureName
     */
    public final void assertNotFlipped(String featureName) {
        assertExist(featureName);
        Assert.assertFalse("'" + featureName + "' is flipped where it shouldn't", ff4j.isFlipped(featureName));
    }

    /**
     * Check Feature Allowed.
     * 
     * @param featureName
     *            target featureName
     */
    public void assertAllowed(String featureName) {
        assertExist(featureName);
        Assert.assertTrue(ff4j.isAllowed(ff4j.getFeature(featureName)));
    }

    /**
     * Check Feature Allowed.
     * 
     * @param featureName
     *            target featureName
     */
    public void assertNotAllowed(String featureName) {
        assertExist(featureName);
        Assert.assertFalse(ff4j.isAllowed(ff4j.getFeature(featureName)));
    }

    /**
     * Check Number of features
     * 
     * @param featureName
     *            target featureName
     */
    public void assertFeatureNumber(int expectedNumber) {
        Assert.assertEquals(expectedNumber, ff4j.getStore().readAll().size());
    }

    /**
     * Check that feature exists and have expected role.
     * 
     * @param featureName
     *            target feature Name
     * @param roleName
     *            target role name
     */
    public void assertHasRole(String featureName, String roleName) {
        assertExist(featureName);
        Assert.assertTrue("'" + featureName + "' has no roles", !ff4j.getFeature(featureName).getAuthorizations().isEmpty());
        Assert.assertTrue("'" + featureName + "' has not role '" + roleName + "'", ff4j.getFeature(featureName)
                .getAuthorizations().contains(roleName));
    }

    /**
     * Check that feature exists and does not have expected role.
     * 
     * @param featureName
     *            target feature Name
     * @param roleName
     *            target role name
     */
    public void assertHasNotRole(String featureName, String roleName) {
        assertExist(featureName);
        Assert.assertFalse(ff4j.getFeature(featureName).getAuthorizations().contains(roleName));
    }

    /**
     * Check that feature is in expected group.
     * 
     * @param featureName
     *            target feature Name
     * @param roleName
     *            target role name
     */
    public void assertInGroup(String featureName, String groupName) {
        assertExist(featureName);
        String group = ff4j.getFeature(featureName).getGroup();
        Assert.assertTrue(group != null && groupName.equals(group));
    }

    /**
     * Chack that feature is enabled in current store.
     * 
     * @param featureName
     *            target featureName
     */
    public void assertEnable(String featureName) {
        Assert.assertTrue(ff4j.getStore().read(featureName).isEnable());
    }

    /**
     * Chack that feature is disabled in current store.
     * 
     * @param featureName
     *            target featureName
     */
    public void assertDisable(String featureName) {
        Assert.assertFalse(ff4j.getStore().read(featureName).isEnable());
    }

    /**
     * Check Group Size
     * 
     * @param expected
     *            expected value for size
     * @param groupName
     *            target groupName
     */
    public void assertGroupSize(int expected, String groupName) {
        Assert.assertEquals(expected, ff4j.getStore().readGroup(groupName).size());
    }

}
