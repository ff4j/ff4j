package org.ff4j.parser.properties;

/*-
 * #%L
 * ff4j-config-properties
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
import org.ff4j.test.unsafe.UnsafeFlippingStrategy;
import org.ff4j.test.unsafe.UnsafeProperty;
import org.junit.Assert;
import org.junit.Test;

public class PropertiesParserTest {
    
    @Test(expected = IllegalArgumentException.class)
    public void propertiesFile_shouldExist() {
        new PropertiesParser().parseConfigurationFile(null);
    }

    @Test
    public void should_export_anyConfig_asProperties() {
        // Given an XML file
        InputStream xmlFile = getClass().getClassLoader().getResourceAsStream("test-ff4j-features.xml");
        // When loading config
        XmlConfig xmlConfig = new XmlParser().parseConfigurationFile(xmlFile);
        // Then it possible to export as YAML
        new PropertiesParser().export(xmlConfig);
    }
    
    @Test
    public void should_fail_for_unsafe_property() {
        // Given a properties file
        InputStream propsFile = getClass().getClassLoader().getResourceAsStream("unsafe/test-ff4j-features.properties");
        // When loading config
        Assert.assertThrows(IllegalArgumentException.class, () -> {
            new PropertiesParser().parseConfigurationFile(propsFile);
        });
        Assert.assertEquals(0, UnsafeProperty.count);
    }
    
    @Test
    public void should_fail_for_unsafe_strategy() {
        // Given a properties file
        InputStream propsFile = getClass().getClassLoader().getResourceAsStream("unsafe/test-ff4j-strategy.properties");
        // When loading config
        Assert.assertThrows(IllegalArgumentException.class, () -> {
            new PropertiesParser().parseConfigurationFile(propsFile);
        });
        Assert.assertEquals(0, UnsafeFlippingStrategy.count);
    }
    
    @Test
    public void importProperties_should_be_same_asXMLImport() {
        // Give XML an YAML files
        InputStream xmlFile = getClass().getClassLoader().getResourceAsStream("test-ff4j-features.xml");
        InputStream ymlFile = getClass().getClassLoader().getResourceAsStream("test-ff4j-features.properties");
        // When parsing those files
        XmlConfig xmlConfig = new XmlParser().parseConfigurationFile(xmlFile);
        FF4jConfiguration propsConfig = new PropertiesParser().parseConfigurationFile(ymlFile);
        // Than both config are even
        assertEquals(xmlConfig.getFeatures().size(), propsConfig.getFeatures().size());
        assertEquals(xmlConfig.getProperties().size(), propsConfig.getProperties().size());
        
        // Custom-properties
        Feature f1Xml = xmlConfig.getFeatures().get("first");
        Feature f1props = propsConfig.getFeatures().get("first");
        assertEquals(f1Xml.getDescription(), f1props.getDescription());
        assertEquals(f1Xml.getCustomProperties().size(), f1props.getCustomProperties().size());
        
        // FlipStrategy & Permission
        Feature f3Xml = xmlConfig.getFeatures().get("third");
        Feature f3Props = propsConfig.getFeatures().get("third");
        assertEquals(
                f3Xml.getFlippingStrategy().getClass(), 
                f3Props.getFlippingStrategy().getClass());
        assertEquals(
                f3Xml.getFlippingStrategy().getInitParams().get("expression"), 
                f3Props.getFlippingStrategy().getInitParams().get("expression"));
        assertEquals(
                f3Xml.getPermissions(), 
                f3Props.getPermissions());
    }

}
