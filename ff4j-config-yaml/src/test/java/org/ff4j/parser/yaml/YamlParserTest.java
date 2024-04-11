package org.ff4j.parser.yaml;

/*-
 * #%L
 * ff4j-config-yaml
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

import static org.junit.Assert.assertEquals;

import java.io.InputStream;

import org.ff4j.conf.FF4jConfiguration;
import org.ff4j.conf.XmlConfig;
import org.ff4j.conf.XmlParser;
import org.ff4j.core.Feature;
import org.ff4j.test.unsafe.UnsafeProperty;
import org.ff4j.test.unsafe.UnsafeFlippingStrategy;
import org.junit.Assert;
import org.junit.Test;

public class YamlParserTest {
    
    @Test(expected = IllegalArgumentException.class)
    public void yamlFile_shouldExist() {
        new YamlParser().parseConfigurationFile(null);
    }
    
    @Test
    public void should_export_anyConfig_asYaml() {
        // Given an XML file
        InputStream xmlFile = getClass().getClassLoader().getResourceAsStream("test-ff4j-features.xml");
        // When loading config
        XmlConfig xmlConfig = new XmlParser().parseConfigurationFile(xmlFile);
        // Then it possible to export as YAML
        new YamlParser().export(xmlConfig);
    }

    @Test
    public void should_fail_for_unsafe_property() {
        // Given a YAML file
        InputStream ymlFile = getClass().getClassLoader().getResourceAsStream("unsafe/test-ff4j-features.yml");
        // When loading config
        Assert.assertThrows(IllegalArgumentException.class, () -> {
            new YamlParser().parseConfigurationFile(ymlFile);
        });
        Assert.assertEquals(0, UnsafeProperty.count);
    }

    @Test
    public void should_fail_for_unsafe_strategy() {
        // Given a YAML file
        InputStream ymlFile = getClass().getClassLoader().getResourceAsStream("unsafe/test-ff4j-strategy.yml");
        // When loading config
        Assert.assertThrows(IllegalArgumentException.class, () -> {
            new YamlParser().parseConfigurationFile(ymlFile);
        });
        Assert.assertEquals(0, UnsafeFlippingStrategy.count);
    }
    
    @Test
    public void importYaml_should_be_same_asXMLImport() {
        // Give XML an YAML files
        InputStream xmlFile = getClass().getClassLoader().getResourceAsStream("test-ff4j-features.xml");
        InputStream ymlFile = getClass().getClassLoader().getResourceAsStream("test-ff4j-features.yml");
        // When parsing those files
        XmlConfig xmlConfig = new XmlParser().parseConfigurationFile(xmlFile);
        FF4jConfiguration ymlConfig = new YamlParser().parseConfigurationFile(ymlFile);
        // Than both config are even
        assertEquals(xmlConfig.getFeatures().size(), ymlConfig.getFeatures().size());
        assertEquals(xmlConfig.getProperties().size(), ymlConfig.getProperties().size());
        
        // Custom-properties
        Feature f1Xml = xmlConfig.getFeatures().get("first");
        Feature f1Yml = ymlConfig.getFeatures().get("first");
        assertEquals(f1Xml.getDescription(), f1Yml.getDescription());
        assertEquals(f1Xml.getCustomProperties().size(), f1Yml.getCustomProperties().size());
        
        // FlipStrategy & Permission
        Feature f3Xml = xmlConfig.getFeatures().get("third");
        Feature f3Yml = ymlConfig.getFeatures().get("third");
        assertEquals(
                f3Xml.getFlippingStrategy().getClass(), 
                f3Yml.getFlippingStrategy().getClass());
        assertEquals(
                f3Xml.getFlippingStrategy().getInitParams().get("expression"), 
                f3Yml.getFlippingStrategy().getInitParams().get("expression"));
        assertEquals(
                f3Xml.getPermissions(), 
                f3Yml.getPermissions());
    }

}
