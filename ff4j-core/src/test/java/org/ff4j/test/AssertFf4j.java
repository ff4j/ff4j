package org.ff4j.test;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.ff4j.FF4j;
import org.ff4j.property.Property;
import org.junit.jupiter.api.Assertions;

/**
 * Give utilities method for tests.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class AssertFf4j {

	/** reference to ff4j context. */
	private final FF4j ff4j;

	private int pause;

	/**
	 * Initialisation with current ff4j context.
	 * 
	 * @param ff4j
	 *            current ff4k context
	 */
	public AssertFf4j(FF4j cff4j) {
		this.ff4j = cff4j;
		this.pause = 0;
	}

	/**
	 * Check existence of the traget feature
	 * 
	 * @param featureName
	 *            targte featurename
	 * @return current object
	 */
	public final AssertFf4j assertThatFeatureExist(String featureName) {
		waitSomeSeconds();
		Assertions.assertTrue(ff4j.exist(featureName), "Feature '" + featureName + "' is mandatory");
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check existence of the traget property
	 * 
	 * @param featureName
	 *            targte featurename
	 * @return current object
	 */
	public final AssertFf4j assertThatPropertyExist(String propertyName) {
		Assertions.assertTrue(ff4j.getPropertiesStore().existProperty(propertyName),
				"Property '" + propertyName + "' is mandatory");
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check inexistence of the target feature
	 * 
	 * @param featureName
	 *            Target featurename
	 * @return current object
	 */
	public final AssertFf4j assertThatFeatureDoesNotExist(String featureName) {
		Assertions.assertFalse(ff4j.exist(featureName), "Feature '" + featureName + "' must not exist");
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check existence of the traget property
	 * 
	 * @param featureName
	 *            targte featurename
	 * @return current object
	 */
	public final AssertFf4j assertThatPropertyDoesNotExist(String propertyName) {
		Assertions.assertFalse(ff4j.getPropertiesStore().existProperty(propertyName),
				"Property '" + propertyName + "' is mandatory");
		waitSomeSeconds();
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
		Assertions.assertTrue(ff4j.check(featureName), "'" + featureName + "' is not flipped where it should");
		waitSomeSeconds();
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
		Assertions.assertFalse(ff4j.check(featureName), "'" + featureName + "' is flipped where it shouldn't");
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check Feature Allowed.
	 * 
	 * @param featureName
	 *            target featureName
	 * @return current object
	 */
	public final AssertFf4j assertThatCurrentUserIsAllowedOnFeature(String featureName) {
		assertThatFeatureExist(featureName);
		Assertions.assertTrue(ff4j.isAllowed(ff4j.getFeature(featureName)));
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check Feature Allowed.
	 * 
	 * @param featureName
	 *            target featureName
	 * @return current object
	 */
	public final AssertFf4j assertThatCurrentUserIsNotAllowedOnFeature(String featureName) {
		assertThatFeatureExist(featureName);
		Assertions.assertFalse(ff4j.isAllowed(ff4j.getFeature(featureName)));
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check Number of features
	 * 
	 * @param featureName
	 *            target featureName
	 * @return current object
	 */
	public final AssertFf4j assertThatStoreHasSize(int expectedNumber) {
		waitSomeSeconds();
		Assertions.assertEquals(expectedNumber, ff4j.getFeatureStore().readAll().size());
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check Number of features
	 * 
	 * @param featureName
	 *            target featureName
	 * @return current object
	 */
	public final AssertFf4j assertThatStoreHasNumberOfGroups(int expectedNumber) {
		Assertions.assertEquals(expectedNumber, ff4j.getFeatureStore().readAllGroups().size());
		waitSomeSeconds();
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
	public final AssertFf4j assertThatFeatureHasRole(String featureName, String roleName) {
		assertThatFeatureExist(featureName);
		Assertions.assertTrue(!ff4j.getFeature(featureName).getPermissions().isEmpty(),
				"'" + featureName + "' has no roles");
		Assertions.assertTrue(ff4j.getFeature(featureName).getPermissions().contains(roleName),
				"'" + featureName + "' has not role '" + roleName + "'");
		waitSomeSeconds();
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
	public final AssertFf4j assertThatFeatureHasNotRole(String featureName, String roleName) {
		assertThatFeatureExist(featureName);
		if (null != ff4j.getFeature(featureName).getPermissions()) {
			Assertions.assertFalse(ff4j.getFeature(featureName).getPermissions().contains(roleName),
					"Feature must no contain role " + roleName);
		}
		waitSomeSeconds();
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
	public final AssertFf4j assertThatFeatureIsInGroup(String featureName, String groupName) {
		assertThatFeatureExist(featureName);
		String group = ff4j.getFeature(featureName).getGroup();
		Assertions.assertTrue(group != null && groupName.equals(group),
				"'" + featureName + "' must be in group '" + groupName + "' but is in <" + group + ">");
		waitSomeSeconds();
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
	public final AssertFf4j assertThatFeatureNotInGroup(String featureName, String groupName) {
		assertThatFeatureExist(featureName);
		String group = ff4j.getFeature(featureName).getGroup();
		Assertions.assertTrue(group == null || !groupName.equals(group));
		waitSomeSeconds();
		return this;
	}

	/**
	 * Chack that feature is enabled in current store.
	 * 
	 * @param featureName
	 *            target featureName
	 * @return current object
	 */
	public final AssertFf4j assertThatFeatureIsEnabled(String featureName) {
		assertThatFeatureExist(featureName);
		Assertions.assertTrue(ff4j.getFeatureStore().read(featureName).isEnable());
		waitSomeSeconds();
		return this;
	}

	/**
	 * Chack that feature is disabled in current store.
	 * 
	 * @param featureName
	 *            target featureName
	 * @return current object
	 */
	public final AssertFf4j assertThatFeatureIsDisabled(String featureName) {
		assertThatFeatureExist(featureName);
		Assertions.assertFalse(ff4j.getFeatureStore().read(featureName).isEnable(),
				"'" + featureName + "' must be disabled");
		waitSomeSeconds();
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
	public final AssertFf4j assertThatGroupExist(String groupName) {
		Assertions.assertTrue(ff4j.getFeatureStore().existGroup(groupName), "Group '" + groupName + " ' does no exist");
		waitSomeSeconds();
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
		Assertions.assertFalse(ff4j.getFeatureStore().existGroup(groupName), "Group '" + groupName + " ' does no exist");
		waitSomeSeconds();
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
	public final AssertFf4j assertThatGroupHasSize(int expected, String groupName) {
		assertThatGroupExist(groupName);
		Assertions.assertEquals(expected, ff4j.getFeatureStore().readGroup(groupName).size());
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check existence of the traget feature
	 * 
	 * @param featureName
	 *            targte featurename
	 * @return current object
	 */
	public final AssertFf4j assertThatFeatureHasFlippingStrategy(String featureName) {
		Assertions.assertNotNull(ff4j.getFeature(featureName).getFlippingStrategy(),
				"Feature '" + featureName + "' must have a FlippingStrategy but doesn't");
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check existence of the traget feature
	 * 
	 * @param featureName
	 *            targte featurename
	 * @return current object
	 */
	public final AssertFf4j assertThatFeatureDoesNotHaveFlippingStrategy(String featureName) {
		Assertions.assertNull(ff4j.getFeature(featureName).getFlippingStrategy(),
				"Feature '" + featureName + "' must not have a flipping strategy");
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check existence of the traget feature
	 * 
	 * @param featureName
	 *            targte featurename
	 * @return current object
	 */
	public final AssertFf4j assertThatFeatureHasProperties(String featureName) {
		assertThatFeatureExist(featureName);
		Map<String, Property<?>> properties = ff4j.getFeature(featureName).getCustomProperties();
		Assertions.assertTrue((properties != null) && (properties.size() > 0), "Properties are required");
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check existence of the traget feature
	 * 
	 * @param featureName
	 *            targte featurename
	 * @return current object
	 */
	public final AssertFf4j assertThatFeatureDoesNotHaveProperties(String featureName) {
		assertThatFeatureExist(featureName);
		Map<String, Property<?>> properties = ff4j.getFeature(featureName).getCustomProperties();
		Assertions.assertTrue((properties == null) || properties.isEmpty(), "Properties are required");
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check existence of the traget feature
	 * 
	 * @param featureName
	 *            targte featurename
	 * @return current object
	 */
	public final AssertFf4j assertThatFeatureHasProperty(String featureName, String propertyName) {
		assertThatFeatureHasProperties(featureName);
		Map<String, Property<?>> properties = ff4j.getFeature(featureName).getCustomProperties();
		Assertions.assertTrue(properties.containsKey(propertyName), "Feature must contain property " + propertyName);
		waitSomeSeconds();
		return this;
	}

	/**
	 * Check existence of the traget feature
	 * 
	 * @param featureName
	 *            targte featurename
	 * @return current object
	 */
	public final AssertFf4j assertThatFeatureHasNotProperty(String featureName, String propertyName) {
		assertThatFeatureExist(featureName);
		Map<String, Property<?>> properties = ff4j.getFeature(featureName).getCustomProperties();
		Assertions.assertTrue((properties == null) || !properties.containsKey(propertyName),
				"Feature must contain property " + propertyName);
		waitSomeSeconds();
		return this;
	}

	// Getters & setters

	public void setPause(int pause) {
		this.pause = pause;
	}

	// Convenient methods

	private void waitSomeSeconds() {
		try {
			TimeUnit.SECONDS.sleep(pause);
		} catch (InterruptedException e) {
			System.out.println(e);
		}
	}
}
