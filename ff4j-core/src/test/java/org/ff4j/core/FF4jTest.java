package org.ff4j.core;

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

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.ff4j.FF4j;
import org.ff4j.Foldable;
import org.ff4j.feature.Feature;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.parser.xml.XmlParserV2;
import org.ff4j.property.PropertyString;
import org.ff4j.property.exception.PropertyNotFoundException;
import org.ff4j.test.AssertFF4j;
import org.ff4j.test.FF4jTestDataSet;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Test operations over {@link FF4j}
 * 
 * @author Cedrick Lunven (@clunven)
 */
@DisplayName("TMain FF4j Test Class")
public class FF4jTest implements FF4jTestDataSet {

    /** FF4J Instance for testings. */
    protected FF4j ff4j = null;

    /** Test Values */
    protected AssertFF4j assertFf4j = null;
    
    /** {@inheritDoc} */
    @BeforeEach
    public void init() {
        ff4j = new FF4j(new XmlParserV2(), "ff4j-testDataSet.xml");
        this.assertFf4j = new AssertFF4j(ff4j);
    }

    @Test
    @DisplayName("When toggling with unkown feature uid, expecting FeatureNotFoundException")
    public void readDataNotAvailableShouldThrowNotFoundException() {
        assertThrows(FeatureNotFoundException.class,  () -> { ff4j.readFeature("i-dont-exist");  });
        assertThrows(PropertyNotFoundException.class, () -> { ff4j.readProperty("i-dont-exist"); });
    }
    
    @Test
    @DisplayName("When saving a feature, it becomes available")
    public void savingFeatureShouldCreateItInRepository() {
        FF4j ff4j = new FF4j().withAudit();
        Assertions.assertFalse(ff4j.getRepositoryFeatures().exists(FEATURE_FOR_TEST));
        ff4j.saveFeature(new Feature(FEATURE_FOR_TEST).toggleOn());
        Assertions.assertTrue(ff4j.getRepositoryFeatures().exists(FEATURE_FOR_TEST));
        ff4j.getRepositoryFeatures().delete(FEATURE_FOR_TEST);
        Assertions.assertFalse(ff4j.getRepositoryFeatures().exists(FEATURE_FOR_TEST));
    }
    
    @Test
    @DisplayName("When saving a property, it becomes available")
    public void savingPropertyShouldCreateEntryInRepository() {
        FF4j ff4j = new FF4j().withAudit();
        Assertions.assertFalse(ff4j.getRepositoryProperties().exists(PROPERTY_FOR_TEST));
        ff4j.saveProperty(new PropertyString(PROPERTY_FOR_TEST, "v1"));
        Assertions.assertTrue(ff4j.getRepositoryProperties().exists(PROPERTY_FOR_TEST));
        ff4j.getRepositoryProperties().delete(PROPERTY_FOR_TEST);
        Assertions.assertFalse(ff4j.getRepositoryProperties().exists(PROPERTY_FOR_TEST));
    }
    
    @Test
    @DisplayName("Toggling on a feature should make the feature enabled")
    public void togglingOnShouldMakeFeatureEnabled() {
        FF4j ff4j = new FF4j().withAudit();
        Assertions.assertFalse(ff4j.getRepositoryFeatures().exists(FEATURE_FOR_TEST));
        ff4j.saveFeature(new Feature(FEATURE_FOR_TEST).toggleOff());
        Assertions.assertFalse(ff4j.check(FEATURE_FOR_TEST));
        ff4j.toggleOn(FEATURE_FOR_TEST);
        Assertions.assertTrue(ff4j.check(FEATURE_FOR_TEST));
    }
    
    @Test
    @DisplayName("Toggling off a feature should make the feature disabled")
    public void togglingOffShouldMakeFeatureDisabled() {
        FF4j ff4j = new FF4j().withAudit();
        Assertions.assertFalse(ff4j.getRepositoryFeatures().exists(FEATURE_FOR_TEST));
        ff4j.saveFeature(new Feature(FEATURE_FOR_TEST).toggleOn());
        Assertions.assertTrue(ff4j.getRepositoryFeatures().exists(FEATURE_FOR_TEST));
        Assertions.assertTrue(ff4j.check(FEATURE_FOR_TEST));
        ff4j.toggleOff(FEATURE_FOR_TEST);
        Assertions.assertFalse(ff4j.check(FEATURE_FOR_TEST));
    }

    @Test
    @DisplayName("Fold over enabled feature will execute existingFunction")
    void foldOverEnabled() {
        FF4j ff4j = new FF4j();
        ff4j.saveFeature(new Feature(FEATURE_FOR_TEST).toggleOn());
        boolean enabled = ff4j.operateWith(FEATURE_FOR_TEST).fold(
                () -> false,
                feature -> true);
        Assertions.assertTrue(enabled);
    }

    @Test
    @DisplayName("Fold over disabled feature will execute non existing Function")
    void foldOverDisabled() {
        FF4j ff4j = new FF4j();
        ff4j.saveFeature(new Feature(FEATURE_FOR_TEST).toggleOff());
        boolean enabled = ff4j.operateWith(FEATURE_FOR_TEST).fold(
                () -> false,
                feature -> true);
        Assertions.assertFalse(enabled);
    }

    @Test
    @DisplayName("Fold over a non existing feature will execute non existing Function")
    void foldOverNonExisting() {
        FF4j ff4j = new FF4j();
        ff4j.saveFeature(new Feature(FEATURE_FOR_TEST).toggleOff());
        boolean enabled = ff4j.operateWith(F1).fold(
                () -> false,
                feature -> true);
        Assertions.assertFalse(enabled);
    }
}
