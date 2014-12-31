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

import org.ff4j.FF4j;
import org.junit.Assert;

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
     * @return current object
     */
    public final AssertFf4j assertThatFeatureExist(String featureName) {
        Assert.assertTrue("Feature '" + featureName + "' is mandatory", ff4j.exist(featureName));
        return this;
    }

    /**
     * Check inexistence of the traget feature
     * 
     * @param featureName
     *            targte featurename
     * @return current object
     */
    public final AssertFf4j assertThatFeatureDoesNotExist(String featureName) {
        Assert.assertFalse("Feature '" + featureName + "' must not exist", ff4j.exist(featureName));
        return this;
    }

    /**
     * Check Feature Flipped
     * 
     * @param featureName
     *            target featureName
     * @return current object
     */
    public final AssertFf4j assertThatFeatureFlipped(String featureName) {
        assertThatFeatureExist(featureName);
        Assert.assertTrue("'" + featureName + "' is not flipped where it should", ff4j.check(featureName));
        return this;
    }

    /**
     * Check Feature Flipped
     * 
     * @param featureName
     *            target featureName
     * @return current object
     */
    public final AssertFf4j assertThatFeatureNotFlipped(String featureName) {
        assertThatFeatureExist(featureName);
        Assert.assertFalse("'" + featureName + "' is flipped where it shouldn't", ff4j.check(featureName));
        return this;
    }

    /**
     * Check Feature Allowed.
     * 
     * @param featureName
     *            target featureName
     * @return current object
     */
    public AssertFf4j assertThatCurrentUserIsAllowedOnFeature(String featureName) {
        assertThatFeatureExist(featureName);
        Assert.assertTrue(ff4j.isAllowed(ff4j.getFeature(featureName)));
        return this;
    }

    /**
     * Check Feature Allowed.
     * 
     * @param featureName
     *            target featureName
     * @return current object
     */
    public AssertFf4j assertThatCurrentUserIsNotAllowedOnFeature(String featureName) {
        assertThatFeatureExist(featureName);
        Assert.assertFalse(ff4j.isAllowed(ff4j.getFeature(featureName)));
        return this;
    }

    /**
     * Check Number of features
     * 
     * @param featureName
     *            target featureName
     * @return current object
     */
    public AssertFf4j assertThatStoreHasSize(int expectedNumber) {
        Assert.assertEquals(expectedNumber, ff4j.getStore().readAll().size());
        return this;
    }

    /**
     * Check Number of features
     * 
     * @param featureName
     *            target featureName
     * @return current object
     */
    public AssertFf4j assertThatStoreHasNumberOfGroups(int expectedNumber) {
        Assert.assertEquals(expectedNumber, ff4j.getStore().readAllGroups().size());
        return this;
    }

    /**
     * Check that feature exists and have expected role.
     * 
     * @param featureName
     *            target feature Name
     * @param roleName
     *            target role name
     * @return current object
     */
    public AssertFf4j assertThatFeatureHasRole(String featureName, String roleName) {
        assertThatFeatureExist(featureName);
        Assert.assertTrue("'" + featureName + "' has no roles", !ff4j.getFeature(featureName).getPermissions().isEmpty());
        Assert.assertTrue("'" + featureName + "' has not role '" + roleName + "'", ff4j.getFeature(featureName)
.getPermissions()
                .contains(roleName));
        return this;
    }

    /**
     * Check that feature exists and does not have expected role.
     * 
     * @param featureName
     *            target feature Name
     * @param roleName
     *            target role name
     * @return current object
     */
    public AssertFf4j assertThatFeatureHasNotRole(String featureName, String roleName) {
        assertThatFeatureExist(featureName);
        if (null != ff4j.getFeature(featureName).getPermissions()) {
            Assert.assertFalse("Feature must no contain role " + roleName, ff4j.getFeature(featureName).getPermissions().contains(roleName));
        }
        return this;
    }

    /**
     * Check that feature is in expected group.
     * 
     * @param featureName
     *            target feature Name
     * @param roleName
     *            target role name
     * @return current object
     */
    public AssertFf4j assertThatFeatureIsInGroup(String featureName, String groupName) {
        assertThatFeatureExist(featureName);
        String group = ff4j.getFeature(featureName).getGroup();
        Assert.assertTrue("'" + featureName + "' must be in group '" + 
                groupName + "' but is in <" + group + ">", 
                group != null && groupName.equals(group));
        return this;
    }

    /**
     * Check that feature is in expected group.
     * 
     * @param featureName
     *            target feature Name
     * @param roleName
     *            target role name
     * @return current object
     */
    public AssertFf4j assertThatFeatureNotInGroup(String featureName, String groupName) {
        assertThatFeatureExist(featureName);
        String group = ff4j.getFeature(featureName).getGroup();
        Assert.assertTrue(group == null || !groupName.equals(group));
        return this;
    }

    /**
     * Chack that feature is enabled in current store.
     * 
     * @param featureName
     *            target featureName
     * @return current object
     */
    public AssertFf4j assertThatFeatureIsEnabled(String featureName) {
        assertThatFeatureExist(featureName);
        Assert.assertTrue(ff4j.getStore().read(featureName).isEnable());
        return this;
    }

    /**
     * Chack that feature is disabled in current store.
     * 
     * @param featureName
     *            target featureName
     * @return current object
     */
    public AssertFf4j assertThatFeatureIsDisabled(String featureName) {
        assertThatFeatureExist(featureName);
        Assert.assertFalse("'" + featureName + "' must be disabled", ff4j.getStore().read(featureName).isEnable());
        return this;
    }

    /**
     * Check Group Size
     * 
     * @param expected
     *            expected value for size
     * @param groupName
     *            target groupName
     * @return current object
     */
    public AssertFf4j assertThatGroupExist(String groupName) {
        Assert.assertTrue("Group '" + groupName + " ' does no exist", ff4j.getStore().existGroup(groupName));
        return this;
    }

    /**
     * Check that group does not exist
     * 
     * @param expected
     *            expected value for size
     * @param groupName
     *            target groupName
     * @return current object
     */
    public AssertFf4j assertThatGroupDoesNotExist(String groupName) {
        Assert.assertFalse("Group '" + groupName + " ' does no exist", ff4j.getStore().existGroup(groupName));
        return this;
    }

    /**
     * Check Group Size
     * 
     * @param expected
     *            expected value for size
     * @param groupName
     *            target groupName
     * @return current object
     */
    public AssertFf4j assertThatGroupHasSize(int expected, String groupName) {
        assertThatGroupExist(groupName);
        Assert.assertEquals(expected, ff4j.getStore().readGroup(groupName).size());
        return this;
    }

}
