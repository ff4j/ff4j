package org.ff4j.parser.yaml;

/*-
 * #%L
 * ff4j-utils-yaml
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

import java.io.InputStream;
import java.util.Map;

import org.ff4j.feature.Feature;
import org.ff4j.feature.togglestrategy.PonderationToggleStrategy;
import org.ff4j.feature.togglestrategy.TogglePredicate;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.security.FF4jPermission;
import org.ff4j.test.FF4jTestDataSet;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Parsing sample files.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@DisplayName("Testing YamlParser configuration files")
public class YamlParserTest implements FF4jTestDataSet {
    
    /** DataSet. **/
    protected FF4jConfigFile testDataSet;
    
    /** {@inheritDoc} */
    @BeforeEach
    public void setUp() throws Exception {
        testDataSet = expectConfig();
    }
    
    @Test
    @DisplayName("Parsing Yaml files and evaluate parts")
    public void testLoaderYAMLFile() {
        // Given
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-testDataset.yml");
        
        // Parsing
        FF4jConfigFile configFile = new YamlParser().parse(in);
        
        // Evaluating Features
        Map<String, Feature> features = configFile.getFeatures();
        
        // Then
        Assertions.assertTrue(features.containsKey(F1));
        Assertions.assertTrue(features.containsKey(F2));
        Assertions.assertTrue(features.containsKey(F3));
        Assertions.assertTrue(features.containsKey(F4));
        Assertions.assertEquals(testDataSet.getFeatures().size(), features.size());
        Feature f4 = features.get(F4);
        Assertions.assertEquals(F4, f4.getUid());
        Assertions.assertTrue(f4.getDescription().isPresent() && !"".equals(f4.getDescription().get()));
        
        Feature f2 = features.get(F2);
        Assertions.assertNotNull(f2);
        Assertions.assertNotNull(f2.getUid());
        // Features -- Properties
        Assertions.assertNotNull(f2.getProperties());
        Assertions.assertNotNull(f2.getProperties().get("ppint"));
        Assertions.assertEquals(12, f2.getProperties().get("ppint").asInt());
        Assertions.assertEquals(12.5, f2.getProperties().get("ppdouble").asDouble());
        Assertions.assertEquals(true, f2.getProperties().get("ppboolean").asBoolean());
        Assertions.assertEquals("hello", f2.getProperties().get("ppstring").asString(), "hello");
        Assertions.assertEquals("NA",    f2.getProperties().get("regionIdentifier").asString());
        Assertions.assertTrue(f2.getProperties().get("regionIdentifier").getFixedValues().isPresent());
        // Features -- ToggleStrategies
        Assertions.assertFalse(f2.getToggleStrategies().isEmpty());
        TogglePredicate tp = f2.getToggleStrategies().get(0);
        Assertions.assertEquals(PonderationToggleStrategy.class.getName(), tp.getClass().getName());
        PonderationToggleStrategy pts = (PonderationToggleStrategy) tp;
        Assertions.assertEquals(new Double(1), 
                pts.getPropertiesAsMap().get(PonderationToggleStrategy.PARAM_WEIGHT).getValue());
        // Features -- Permissions
        Assertions.assertFalse(f2.getAccessControlList().getPermissions().isEmpty());
        Assertions.assertTrue(f2.getAccessControlList().getPermissions().containsKey(FF4jPermission.FEATURE_TOGGLE));
        
        // roles
        Assertions.assertNotNull(configFile.getRoles());
        Assertions.assertEquals(4, configFile.getRoles().size());
        Assertions.assertNotNull(configFile.getRoles().get("USER"));
        
        // users
        Assertions.assertNotNull(configFile.getUsers());
        Assertions.assertEquals(2, configFile.getUsers().size());
        Assertions.assertEquals(1, configFile.getUsers().get("john").getRoles().size());

        // Properties
        Assertions.assertNotNull(configFile.getProperties());
        Assertions.assertEquals(32, configFile.getProperties().size());
    }
}
    
