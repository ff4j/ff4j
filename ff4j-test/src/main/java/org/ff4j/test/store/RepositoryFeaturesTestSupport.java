package org.ff4j.test.store;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2018 FF4J
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

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.ff4j.FF4j;
import org.ff4j.exception.AssertionViolationException;
import org.ff4j.feature.Feature;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.feature.exception.GroupNotFoundException;
import org.ff4j.feature.repository.FeaturesRepository;
import org.ff4j.feature.togglestrategy.PonderationToggleStrategy;
import org.ff4j.parser.ConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyBoolean;
import org.ff4j.property.PropertyInt;
import org.ff4j.test.AssertFF4j;
import org.ff4j.test.FF4jTestDataSet;
import org.ff4j.utils.Util;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Abtract class used to test multiple implementation of {@link FeaturesRepository} in order to behave the
 * same.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class RepositoryFeaturesTestSupport implements FF4jTestDataSet {

    /** Initialize */
	protected FF4j ff4j = null;

	/** Tested Store. */
	protected FeaturesRepository testedStore;

	/** Test Values */
	protected AssertFF4j assertFF4j;
	
	/** DataSet. **/
	protected FF4jConfigFile testDataSet;
	
	/** {@inheritDoc} */
	@BeforeEach
	public void setUp() throws Exception {
	    ConfigurationFileParser.clearCache();
	    ff4j        = new FF4j().repositoryFeatures(initStore());
		assertFF4j  = new AssertFF4j(ff4j);
		testedStore = ff4j.getRepositoryFeatures();
        testDataSet = expectConfig();
	}

	/**
	 * Any store test will declare its store through this callback.
	 * 
	 * @return working feature store
	 * @throws Exception
	 *          Hi guys, just let you know I did the update in the presentation : changing instructors names to put the 2 of you    error during building feature store
	 */
	protected abstract FeaturesRepository initStore();

	@Test
	@DisplayName("When configuration file is null, expecting violation exception")
	public void readWithNullShouldThrowViolationException() {
	    assertThrows(AssertionViolationException.class, () -> { testedStore.read(null); });
	}

	@Test
	@DisplayName("When configuration file is empty, expecting violation exception")
	public void readWithEmptyShouldThrowViolationException() {
        assertThrows(AssertionViolationException.class, () -> { testedStore.read(""); });
	}
	
	@Test
    @DisplayName("When parsing configuration file, should have expected test DataSet")
    public void readAllConfFileShouldMatchExpectedDataSet() {
        // Given
        assertFF4j.assertThatStoreHasSize(testDataSet.getFeatures().size());
        assertFF4j.assertThatFeatureExist(F1);
        assertFF4j.assertThatFeatureExist(F2);
        assertFF4j.assertThatFeatureExist(F3);
        assertFF4j.assertThatFeatureExist(F4);
        // When
        Map < String, Feature > features = Util.toMap(testedStore.findAll());
        // Then
        Assertions.assertEquals(testDataSet.getFeatures().size(), features.size());
        Feature f4 = testedStore.read(F4);
        Assertions.assertEquals(F4, f4.getUid());
        Assertions.assertTrue(f4.getDescription().isPresent() && !"".equals(f4.getDescription().get()));
        assertFF4j.assertThatFeatureIsInGroup(F4, GRP1);
    }
	
	@Test
	@DisplayName("When accessing unkown feature uid, expecting FeatureNotFoundException")
	public void readWithUnknownFeatureShouldThrowFeatureNotFoundException() {
        assertThrows(FeatureNotFoundException.class, () -> { testedStore.read("do-not-exist"); });
	}

	@Test
	@DisplayName("When reading feature from dataSet, everything must have been populated")
	public void readFeatureFromConfFileShouldMatchExpectedDataSet() {
		// Given
	    Feature expectedF2 = testDataSet.getFeatures().get(F2);
	    // When
	    assertFF4j.assertThatFeatureExist(F2);
		Feature f2 = testedStore.read(F2);
		// uid
		Assertions.assertNotNull(f2);
		Assertions.assertEquals(f2.getUid(), expectedF2.getUid());
		// description
        Assertions.assertTrue(f2.getDescription().isPresent());
		Assertions.assertEquals(f2.getDescription().get(), f2.getDescription().get());
		// groupName
		Assertions.assertTrue(f2.getGroup().isPresent());
		Assertions.assertEquals(f2.getGroup().get(), f2.getGroup().get());
		// permissions
		Assertions.assertTrue(f2.getAccessControlList().getPermissions() != null && 
                !f2.getAccessControlList().getPermissions().isEmpty());
        // strategies
		Assertions.assertFalse(f2.getToggleStrategies().isEmpty());
		Assertions.assertEquals(
		            expectedF2.getToggleStrategies().get(0).getClass(), 
		            f2.getToggleStrategies().get(0).getClass());
		Assertions.assertEquals(
		   expectedF2.getToggleStrategies().get(0).getParams().get(PonderationToggleStrategy.PARAM_WEIGHT), 
		           f2.getToggleStrategies().get(0).getParams().get(PonderationToggleStrategy.PARAM_WEIGHT));
        // properties
		Assertions.assertFalse(f2.getProperties().isEmpty());
		Assertions.assertEquals(expectedF2.getProperties().size(), f2.getProperties().size());
		Assertions.assertTrue(f2.getProperties().containsKey(P_F2_PPINT));
		Assertions.assertTrue(f2.getProperties().containsKey(P_F2_PDOUBLE));
		assertFF4j.assertThatFeatureIsInGroup(F2, GRP1);
	}

	@Test
	@DisplayName("When toggling  with null feature uid, expecting AssertionViolationException")
	public void toggleWithNullShouldThrowViolationException() {
	    assertThrows(AssertionViolationException.class, () -> { testedStore.toggleOn(null); });
	    assertThrows(AssertionViolationException.class, () -> { testedStore.toggleOff(null); });
	}
	
	@Test
	@DisplayName("When toggling  with empty feature uid, expecting AssertionViolationException")
	public void toggleWithEmptyShouldThrowViolationException() {
	    assertThrows(AssertionViolationException.class, () -> { testedStore.toggleOn(""); });
	    assertThrows(AssertionViolationException.class, () -> { testedStore.toggleOff(""); });
        
	}
	
	@Test
	@DisplayName("When toggling with unkown feature uid, expecting FeatureNotFoundException")
	public void toggleWithUnknownFeatureShouldThrowFeatureNotFoundException() {
		assertThrows(FeatureNotFoundException.class, () -> { testedStore.toggleOn("does-not-exist"); });
		assertThrows(FeatureNotFoundException.class, () -> { testedStore.toggleOff("does-not-exist"); });
	}
	
	@Test
	@DisplayName("When toggling ON/OFF feature, expecting feature has been toggled")
	public void toggleFeatureShouldChangeFeatureStatus() {
		// Given
		assertFF4j.assertThatFeatureExist(F1);
		assertFF4j.assertThatFeatureIsDisabled(F1);
		// When (1)
		testedStore.toggleOn(F1);
		assertFF4j.assertThatFeatureIsEnabled(F1);
		// When (2)
		testedStore.toggleOff(F1);
		assertFF4j.assertThatFeatureIsDisabled(F1);
	}
	
	@Test
	@DisplayName("When creating feature with null param, expecting AssertionViolationException")
	public void createWithNullShouldThrowViolationException() throws Exception {
	    assertThrows(AssertionViolationException.class, () -> { testedStore.saveFeature(null); });
	}
	
	@Test
	@DisplayName("When creating new feature, it becomes available")
	public void createShouldMakeNewFeatureAvailable() throws Exception {
		assertFF4j.assertThatFeatureDoesNotExist(FEATURE_FOR_TEST);
		Feature fp = new Feature(FEATURE_FOR_TEST).toggleOn().description("description").group(GRP1);
		testedStore.save(fp);
		assertFF4j.assertThatStoreHasSize(testDataSet.getFeatures().size() + 1);
		assertFF4j.assertThatFeatureExist(FEATURE_FOR_TEST);
		assertFF4j.assertThatFeatureIsInGroup(FEATURE_FOR_TEST, GRP1);
	}
	
    @Test
    @DisplayName("When deleting new feature, it is not available anymore")
    public void deleteShouldRemoveFeatureAvailable() throws Exception {
        assertFF4j.assertThatFeatureExist(F1);
        testedStore.delete(F1);
        assertFF4j.assertThatStoreHasSize(testDataSet.getFeatures().size() - 1);
        assertFF4j.assertThatFeatureDoesNotExist(F1);
    }
	
	@Test
	@DisplayName("When deleting feature with null param, expecting AssertionViolationException")
	public void deleteFeatureWithNullShouldThrowViolationException() throws Exception {
	    assertThrows(AssertionViolationException.class, () -> { testedStore.deleteFeature((String) null); });
	}
	
	@Test
	@DisplayName("When deleting feature with null param, expecting FeatureNotFoundException")
	public void deleteUnknownFeatureShouldThrowFeatureNotFound() throws Exception {
	    assertFF4j.assertThatFeatureDoesNotExist(FEATURE_FOR_TEST);
	    assertThrows(FeatureNotFoundException.class, () -> { 
	        testedStore.delete(FEATURE_FOR_TEST); 
	    });
	}

	@Test
    @DisplayName("When updating feature with null param, expecting AssertionViolationException")
    public void updateFeatureWithNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { 
            testedStore.save((Feature) null); 
        });
    }
	
	@Test
    @DisplayName("When updating feature with unknowm param, creating the feature")
    public void updatedUnknownFeatureShouldThrowFeatureNotFound() throws Exception {
        assertFF4j.assertThatFeatureDoesNotExist(FEATURE_FOR_TEST);
        testedStore.save(new Feature(FEATURE_FOR_TEST));
        assertFF4j.assertThatFeatureExist(FEATURE_FOR_TEST);
    }

	@Test
	@DisplayName("When updating feature, metadata should be updated")
	public void testUpdateFeatureCoreData() {
	    // Givens
	    String newDescription = "new-description";
        assertFF4j.assertThatFeatureExist(F1);
	    Assertions.assertFalse(newDescription.equals(testedStore.read(F1).getDescription().get()));
	    Assertions.assertTrue(testedStore.read(F1).getToggleStrategies().isEmpty());
		// When
		Feature fpBis = testedStore.read(F1);
		fpBis.setDescription(newDescription);
		fpBis.getToggleStrategies().add(new PonderationToggleStrategy(0.12));
		testedStore.save(fpBis);
		// Then
		Feature updatedFeature = testedStore.read(F1);
		Assertions.assertTrue(newDescription.equals(updatedFeature.getDescription().get()));
		Assertions.assertFalse(testedStore.read(F1).getToggleStrategies().isEmpty());
		Assertions.assertEquals(
		        testedStore.read(F1).getToggleStrategies().get(0), 
		        updatedFeature.getToggleStrategies().get(0));
	}

	@Test
    @DisplayName("When test feature existence with null param, expecting AssertionViolationException")
    public void existWithNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.exists(null); });
        assertThrows(AssertionViolationException.class, () -> { testedStore.exists(""); });
    }
	
	@Test
    @DisplayName("When test group existence with null param, expecting AssertionViolationException")
    public void existGroupWitNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.existGroup(null); });
        assertThrows(AssertionViolationException.class, () -> { testedStore.existGroup(""); });
    }
	

	@Test
	@DisplayName("When group exist should return true and false if not exists")
	public void existGroupShouldReturnGroupExistenceStatus() {
		// Given
	    assertFF4j.assertThatGroupExist(GRP1);
	    assertFF4j.assertThatGroupDoesNotExist("GRPY");
		// Then
		Assertions.assertTrue(testedStore.existGroup(GRP1));
		Assertions.assertFalse(testedStore.existGroup("GRPY"));
	}
	
	@Test
	@DisplayName("When toggle group with null param, expecting AssertionViolationException")
    public void toggleGroupWitEmptyShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.toggleOnGroup(""); });
        assertThrows(AssertionViolationException.class, () -> { testedStore.toggleOffGroup(""); });
        assertThrows(AssertionViolationException.class, () -> { testedStore.toggleOnGroup(null); });
        assertThrows(AssertionViolationException.class, () -> { testedStore.toggleOffGroup(null); });
    }

	@Test
    @DisplayName("When enable group all features should be enabled")
	public void toggleOffGroupShouldToggleAllfeaturesOfGroup() {
		// Given
	    assertFF4j.assertThatGroupExist(GRP1);
	    assertFF4j.assertThatFeatureIsInGroup(F2, GRP1);
	    assertFF4j.assertThatFeatureIsInGroup(F4, GRP1);
	    assertFF4j.assertThatFeatureIsEnabled(F2);
	    assertFF4j.assertThatFeatureIsEnabled(F4);
	    // When
		testedStore.toggleOffGroup(GRP1);
		// Then
		assertFF4j.assertThatFeatureIsDisabled(F2);
	    assertFF4j.assertThatFeatureIsDisabled(F4);
	    // WHen
	    testedStore.toggleOnGroup(GRP1);
	    // Then
	    assertFF4j.assertThatFeatureIsEnabled(F2);
        assertFF4j.assertThatFeatureIsEnabled(F4);
	}
	
	@Test
    @DisplayName("When enabling unknown group, expecting GroupNotFoundException ")
	public void toggleUnknownGroupShouldThrowGroupNotFoundException() {
	    assertFF4j.assertThatGroupDoesNotExist(GRPX);
	    assertThrows(GroupNotFoundException.class, () -> { testedStore.toggleOnGroup(GRPX); });
	    assertThrows(GroupNotFoundException.class, () -> { testedStore.toggleOffGroup(GRPX); });
	}

	@Test
    @DisplayName("When reading group with null or empty, expecting AssertionViolationException")
    public void readGroupWitNullShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.readGroup(null); });
        assertThrows(AssertionViolationException.class, () -> { testedStore.readGroup(""); });
    }
	
	@Test
    @DisplayName("When reading group, returning all features in the group")
	public void readGroupShouldReturnAllGroupElements() {
		// Given
	    assertFF4j.assertThatGroupExist(GRP1);
	    assertFF4j.assertThatFeatureIsInGroup(F2, GRP1);
        assertFF4j.assertThatFeatureIsInGroup(F4, GRP1);
		// When
		List <Feature> group = testedStore.readGroup(GRP1).collect(Collectors.toList());
		// Then
		Assertions.assertEquals(2, group.size());
		Assertions.assertTrue(group.stream().anyMatch(f -> F2.equals(f.getUid())));
		Assertions.assertTrue(group.stream().anyMatch(f -> F4.equals(f.getUid())));
	}
	
	@Test
    @DisplayName("When reading unknown group, expecting GroupNotFoundException ")
    public void readUnknownGroupShouldThrowGroupNotFoundException() {
	    assertFF4j.assertThatGroupDoesNotExist(GRPX);
        assertThrows(GroupNotFoundException.class, () -> { testedStore.readGroup(GRPX); });
    }

	@Test
    @DisplayName("When adding to groupNull should throw ViolationException")
	public void addingToNullOrEmptyGroupShouldThrowViolationException() throws Exception {
	    assertThrows(AssertionViolationException.class, () -> { testedStore.addToGroup(null, GRP1); });
	    assertThrows(AssertionViolationException.class, () -> { testedStore.addToGroup("", GRP1); });
	    assertThrows(AssertionViolationException.class, () -> { testedStore.addToGroup(F1, null); });
	    assertThrows(AssertionViolationException.class, () -> { testedStore.addToGroup(F1, ""); });
	}
	
	@Test
    @DisplayName("When adding unknow feature to group should throw FeatureNotFound")
    public void addingUknownFeatureToGroupThrowFeatureNotFound() throws Exception {
	    assertThrows(FeatureNotFoundException.class, () -> { testedStore.addToGroup(FEATURE_FOR_TEST, GRP1); });
	}
	
	@Test
    @DisplayName("When adding to feature to unknow group, create group")
    public void addingFeatureToUknownGroupCreateTheGroup() throws Exception {
	    // Given
        assertFF4j.assertThatGroupDoesNotExist(GRPX);
        // When
        testedStore.addToGroup(F1, GRPX);
        // Then
        assertFF4j.assertThatGroupExist(GRPX);
        assertFF4j.assertThatFeatureIsInGroup(F1, GRPX);
        assertFF4j.assertThatGroupHasSize(1, GRPX);
    }
	
	@Test
    @DisplayName("When removing from group Null should throw ViolationException")
    public void removingToNullOrEmptyGroupShouldThrowViolationException() throws Exception {
        assertThrows(AssertionViolationException.class, () -> { testedStore.removeFromGroup(null, GRP1); });
        assertThrows(AssertionViolationException.class, () -> { testedStore.removeFromGroup("", GRP1); });
        assertThrows(AssertionViolationException.class, () -> { testedStore.removeFromGroup(F1, null); });
        assertThrows(AssertionViolationException.class, () -> { testedStore.removeFromGroup(F1, ""); });
    }
	
	@Test
    @DisplayName("When removing from group, group should be empty and size decrease from 1")
	public void removingFromGroupShouldUpdateFeatureAndGroup() throws Exception {
	    // Given
	    assertFF4j.assertThatGroupExist(GRP1);
	    assertFF4j.assertThatGroupHasSize(2, GRP1);
	    assertFF4j.assertThatFeatureIsInGroup(F2, GRP1);
	    // When
	    testedStore.removeFromGroup(F2, GRP1);
	    // Then
	    assertFF4j.assertThatGroupHasSize(1, GRP1);
	    Assertions.assertFalse(testedStore.read(F2).getGroup().isPresent());
	}

	@Test
    @DisplayName("When removing last item of a group remove group")
    public void removingLastFeatureOfGroupDeleteGroup() {
		// Given
	    assertFF4j.assertThatGroupExist(GRP0);
        assertFF4j.assertThatGroupHasSize(1, GRP0);
        assertFF4j.assertThatFeatureIsInGroup(F3, GRP0);
        // When
        testedStore.removeFromGroup(F3, GRP0);
        // Then
        assertFF4j.assertThatGroupDoesNotExist(GRP0);
        Assertions.assertFalse(testedStore.read(F3).getGroup().isPresent());
	}

	@Test
    @DisplayName("When removing a feature does not exist throw FeatureNotFoundException")
	public void removingUnknownFeatureFromGroupThrowFeatureNotFound() {
		// Given
	    assertFF4j.assertThatGroupExist(GRP0);
	    assertFF4j.assertThatFeatureDoesNotExist(FEATURE_FOR_TEST);
	    assertThrows(FeatureNotFoundException.class, () -> { 
	        testedStore.removeFromGroup(FEATURE_FOR_TEST, GRP0); });
	}
	
	@Test
    @DisplayName("When removing a feature from unknown group throw GroupNotFound")
    public void removingFeatureFromUnknownGroupTHrowGroupNotFound() {
        assertFF4j.assertThatGroupDoesNotExist(GRPX);
        assertFF4j.assertThatFeatureExist(F1);
        assertThrows(GroupNotFoundException.class, () -> { 
            testedStore.removeFromGroup(F1, GRPX); });
    }
	
	@Test
    @DisplayName("When removing a feature from wrong group is OK")
    public void removingFeatureFromWrongGroupIsOK() {
        // Given
	    assertFF4j.assertThatGroupExist(GRP0);
        assertFF4j.assertThatGroupExist(GRP1);
        assertFF4j.assertThatFeatureExist(F2);
        assertFF4j.assertThatFeatureIsInGroup(F2, GRP1);
        // When
        testedStore.removeFromGroup(F2, GRP0);
        // Then
        assertFF4j.assertThatFeatureIsInGroup(F2, GRP1);
	}

	@Test
    @DisplayName("When read all groups everything is there")
	public void readingAllGroupsFromTestDataSets() {
		// Given
	    assertFF4j.assertThatStoreHasNumberOfGroups(2);
	    assertFF4j.assertThatGroupExist(GRP0);
	    assertFF4j.assertThatGroupExist(GRP1);
		// When
		Set<String> groups = testedStore.listGroupNames()
		                                .collect(Collectors.toSet());
		// Then
		Assertions.assertEquals(2, groups.size());
		Assertions.assertTrue(groups.contains(GRP0));
		Assertions.assertTrue(groups.contains(GRP1));
	}

	@Test
    @DisplayName("When updating toggle strategies store is updated")
	public void updatingToggleStrategiesShouldUpdateStore() {
		// Given
	    assertFF4j.assertThatFeatureExist(F3);
	    Feature startFeature = testedStore.read(F3);
	    Assertions.assertTrue(startFeature.getToggleStrategies().isEmpty());
		// When
	    startFeature.addToggleStrategy(new PonderationToggleStrategy(1));
		testedStore.save(startFeature);
		// Then
		assertFF4j.assertThatFeatureHasFlippingStrategy(F3);
		Feature endFeature = testedStore.read(F3);
		Assertions.assertFalse(startFeature.getToggleStrategies().isEmpty());
		Assertions.assertEquals(PonderationToggleStrategy.class, 
		        endFeature.getToggleStrategies().get(0).getClass());
	}

	@Test
    @DisplayName("When removing toggle strategies store is updated")
	public void testUpdateRemoveFlippingStrategy() {
		// Given
	    assertFF4j.assertThatFeatureExist(F4);
        Feature startFeature = testedStore.read(F4);
        Assertions.assertFalse(startFeature.getToggleStrategies().isEmpty());
        // When
        startFeature.getToggleStrategies().remove(0);
        testedStore.save(startFeature);
        // Then
        Feature endFeature = testedStore.read(F3);
        Assertions.assertTrue(endFeature.getToggleStrategies().isEmpty());
	}

	@Test
    @DisplayName("When invoking clear all features are deleted")
	public void testClear() {
	    assertFF4j.assertThatFeatureExist(F1);
	    assertFF4j.assertThatFeatureExist(F2);
	    assertFF4j.assertThatFeatureExist(F3);
	    assertFF4j.assertThatFeatureExist(F4);
		testedStore.deleteAll();
		// Then
		Assertions.assertEquals(0, testedStore.findAll().count());
	}

	@Test
    @DisplayName("When updating properties store is updated")
    public void addingPropertiesShouldUpdateStore() {
        // Given
        assertFF4j.assertThatFeatureExist(F3);
        Feature startFeature = testedStore.read(F3);
        Assertions.assertTrue(startFeature.getProperties().isEmpty());
        // When
        startFeature.addProperty(new PropertyBoolean(PROP_FLAG, true));
        testedStore.save(startFeature);
        // Then
        assertFF4j.assertThatFeatureHasProperties(F3);
        assertFF4j.assertThatFeatureHasProperty(F3, PROP_FLAG);
        Feature endFeature = testedStore.read(F3);
        Assertions.assertFalse(startFeature.getProperties().isEmpty());
        Assertions.assertEquals(PropertyBoolean.class, 
                endFeature.getProperties().get(PROP_FLAG).getClass());
	}
	
	@Test
    @DisplayName("When removing properties store is updated")
    public void removingPropertiesShouldUpdateStore() {
        // Given
        assertFF4j.assertThatFeatureExist(F2);
        Feature startFeature = testedStore.read(F2);
        Assertions.assertFalse(startFeature.getProperties().isEmpty());
        // When
        startFeature.getProperties().clear();
        testedStore.save(startFeature);
        // Then
        Feature endFeature = testedStore.read(F2);
        Assertions.assertTrue(endFeature.getProperties().isEmpty());
    }
	
	@Test
    @DisplayName("When updating properties store is updated")
    public void updatingPropertyValueShouldUpdateStore() {
        // Given
        assertFF4j.assertThatFeatureExist(F2);
        Feature startFeature = testedStore.read(F2);
        Assertions.assertFalse(startFeature.getProperties().isEmpty());
        assertFF4j.assertThatFeatureHasProperty(F2, P_F2_PPINT);
        Assertions.assertEquals(12,startFeature.getProperties().get(P_F2_PPINT).asInt());
        // When
        startFeature.getProperties().get(P_F2_PPINT).setValueFromString("20");
        testedStore.save(startFeature);
        // Then
        Feature endFeature = testedStore.read(F2);
        Assertions.assertEquals(20, endFeature.getProperties().get(P_F2_PPINT).asInt());        
    }

    @Test
    @DisplayName("When adding properties fixed values store is updated")
	public void testUpdateEditPropertyAddFixedValues() {
        // Given
        assertFF4j.assertThatFeatureExist(F2);
        Feature startFeature    = testedStore.read(F2);
        assertFF4j.assertThatFeatureHasProperty(F2, P_F2_DIGITVALUE);
        Optional<Property<?>> opDigitValue = startFeature.getProperty(P_F2_DIGITVALUE);
        Assertions.assertTrue(opDigitValue.isPresent());
        PropertyInt pDigitValue = (PropertyInt) opDigitValue.get();
        Assertions.assertEquals(4, pDigitValue.getFixedValues().get().size()); 
        // WHen
        pDigitValue.addFixedValue(4);
        testedStore.save(startFeature);
        // Then
        Feature endFeature = testedStore.read(F2);
        Optional<Property<?>> opt2 = endFeature.getProperty(P_F2_DIGITVALUE);
        PropertyInt digit2 = (PropertyInt) opt2.get();
        Assertions.assertEquals(5, digit2.getFixedValues().get().size()); 
	}

    @Test
    @DisplayName("When removing properties fixed values store is updated")
    public void testUpdateEditPropertyREmoveFixedValues() {
        // Given
        assertFF4j.assertThatFeatureExist(F2);
        Feature startFeature    = testedStore.read(F2);
        assertFF4j.assertThatFeatureHasProperty(F2, P_F2_DIGITVALUE);
        Optional<Property<?>> opDigitValue = startFeature.getProperty(P_F2_DIGITVALUE);
        Assertions.assertTrue(opDigitValue.isPresent());
        PropertyInt pDigitValue = (PropertyInt) opDigitValue.get();
        Assertions.assertEquals(4, pDigitValue.getFixedValues().get().size()); 
        // WHen
        pDigitValue.getFixedValues().get().remove(0);
        pDigitValue.getFixedValues().get().remove(1);
        pDigitValue.getFixedValues().get().remove(2);
        testedStore.save(startFeature);
        // Then
        Feature endFeature = testedStore.read(F2);
        Optional<Property<?>> opt2 = endFeature.getProperty(P_F2_DIGITVALUE);
        PropertyInt digit2 = (PropertyInt) opt2.get();
        Assertions.assertEquals(1, digit2.getFixedValues().get().size()); 
    }
	
}
