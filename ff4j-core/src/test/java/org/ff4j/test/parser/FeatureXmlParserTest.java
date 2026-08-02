package org.ff4j.test.parser;

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

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import org.ff4j.conf.XmlConfig;
import org.ff4j.conf.XmlParser;
import org.ff4j.conf.XmlParserErrorHandler;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * Unit Testing
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class FeatureXmlParserTest {
    @Test
    public void testLoaderXMLFile() {
        InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-ok.xml");
        Map<String, Feature> features = new XmlParser().parseConfigurationFile(in).getFeatures();

        Assertions.assertEquals(7, features.size());
        Assertions.assertTrue(features.containsKey("f0"));
        Assertions.assertNotNull(features.get("f0").getDescription());
        Assertions.assertNotNull(features.get("f0").getPermissions());
        Assertions.assertEquals(2, features.get("f0").getPermissions().size());
        Assertions.assertNotNull(features.get("f0").getFlippingStrategy());
        Assertions.assertEquals(1, features.get("f0").getFlippingStrategy().getInitParams().size());
        Assertions.assertNotNull(features.get("f0").getGroup());
        Assertions.assertEquals("group3", features.get("f0").getGroup());
        Assertions.assertTrue(features.containsKey("f1"));
        Assertions.assertTrue(features.containsKey("f2"));
        Assertions.assertTrue(features.containsKey("f3"));
        Assertions.assertTrue(features.containsKey("f4"));
        Assertions.assertTrue(features.containsKey("f5"));
    }

    @Test
    public void testSaxException() {
        assertThrows(IllegalArgumentException.class, () -> {
            InputStream in = new ByteArrayInputStream("<TOTO>Invalid</TOTO2>".getBytes());
            new XmlParser().parseConfigurationFile(in);
        });
    }

    @Test
    public void testNullFile() {
        assertThrows(IllegalArgumentException.class, () ->
            new XmlParser().parseConfigurationFile(null));
    }

    @Test
    public void testLoaderRequiredUid() {
        assertThrows(IllegalArgumentException.class, () -> {
            InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-ko-uidrequired.xml");
            new XmlParser().parseConfigurationFile(in);
        });
    }

    @Test
    public void testLoaderRequiredEnable() {
        assertThrows(IllegalArgumentException.class, () -> {
            InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-ko-enablerequired.xml");
            new XmlParser().parseConfigurationFile(in);
        });
    }

    @Test
    public void testLoaderLoadInvalidStream() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> {
            InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-ok.xml");
            in.close();
            new XmlParser().parseConfigurationFile(in);
        });
    }

    @Test
    public void importThenExport() throws IOException {
        // Given
        XmlParser parser = new XmlParser();
        InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-import-export.xml");
        Map<String, Feature> features = parser.parseConfigurationFile(in).getFeatures();
        Assertions.assertNotNull(features);
        // When
        InputStream in2 = parser.exportFeatures(features);
        // Then
        // output is OK
        Map<String, Feature> features2 = parser.parseConfigurationFile(in2).getFeatures();
        Assertions.assertNotNull(features2);
        Assertions.assertEquals(features.size(), features2.size());
    }
    
    @Test
    public void importThenExportFeatures2() throws IOException {
        // Given
        XmlParser parser = new XmlParser();
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-parser-all.xml");
        Map<String, Feature> features = parser.parseConfigurationFile(in).getFeatures();
        Assertions.assertNotNull(features);
        // When
        InputStream in3 = parser.exportFeatures(features);
        // Then
        // output is OK
        Map<String, Feature> features2 = parser.parseConfigurationFile(in3).getFeatures();
        Assertions.assertNotNull(features2);
        Assertions.assertEquals(features.size(), features2.size());
    }
    
    @Test
    public void importThenExportALL() throws IOException {
        // Given
        XmlParser parser = new XmlParser();
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-parser-all.xml");
        XmlConfig conf = parser.parseConfigurationFile(in);
        Assertions.assertNotNull(conf.getFeatures());
        Assertions.assertNotNull(conf.getProperties());
        // When
        InputStream in3 = parser.exportAll(conf);
        // Then
        XmlConfig conf2 = parser.parseConfigurationFile(in3);
        Assertions.assertNotNull(conf2.getFeatures());
        Assertions.assertNotNull(conf2.getProperties());
    }
    
    @Test
    public void testPropertiesParsing() throws IOException {
        // Given
        XmlParser parser = new XmlParser();
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j.xml");
        
        // When
        XmlConfig conf = parser.parseConfigurationFile(in);
        // Then
        Map<String, Feature> features = conf.getFeatures();
        Assertions.assertNotNull(features);
        Feature f = features.get("first");
        Assertions.assertNotNull(f);
        Assertions.assertNotNull(f.getUid());
        Assertions.assertNotNull(f.getCustomProperties());
        Assertions.assertNotNull(f.getCustomProperties().get("ppint"));
        Assertions.assertEquals(f.getCustomProperties().get("ppint").asInt(), 12);
        Assertions.assertEquals(f.getCustomProperties().get("ppdouble").asDouble(), 12.5,0);
        Assertions.assertEquals(f.getCustomProperties().get("ppboolean").asBoolean(),true);
        Assertions.assertEquals(f.getCustomProperties().get("ppstring").asString(), "hello");
        Assertions.assertEquals(f.getCustomProperties().get("regionIdentifier").asString(), "AMER");
        Assertions.assertNotNull(f.getCustomProperties().get("regionIdentifier").getFixedValues());
        Assertions.assertFalse(f.getCustomProperties().get("regionIdentifier").getFixedValues().isEmpty());
        PropertyLogLevel pll = (PropertyLogLevel) f.getCustomProperties().get("myLogLevel");
        Assertions.assertEquals(pll.getValue(), LogLevel.DEBUG);
        
        // Then
        Map < String, Property<?>> properties = conf.getProperties();
        Assertions.assertNotNull(properties);
    }
    
    @Test
    public void testParsingALL() throws IOException {
        // Given
        XmlParser parser = new XmlParser();
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-parser-all.xml");
        // When
        XmlConfig conf = parser.parseConfigurationFile(in);
        // Then
        Map<String, Feature> features = conf.getFeatures();
        Assertions.assertNotNull(features);
        // Then
        Map < String, Property<?>> properties = conf.getProperties();
        Assertions.assertNotNull(properties);
    }
    
    @Test
    public void testParsingFeatures() throws IOException {
        // Given
        XmlParser parser = new XmlParser();
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-parser-features.xml");
        // When
        XmlConfig conf = parser.parseConfigurationFile(in);
        // Then
        Map<String, Feature> features = conf.getFeatures();
        Assertions.assertNotNull(features);
        // Then
        Map < String, Property<?>> properties = conf.getProperties();
        Assertions.assertNotNull(properties);
    }
    
    
    @Test
    public void testParsingProperties() throws IOException {
        // Given
        XmlParser parser = new XmlParser();
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-parser-properties.xml");
        // When
        XmlConfig conf = parser.parseConfigurationFile(in);
        // Then
        Map<String, Feature> features = conf.getFeatures();
        Assertions.assertNotNull(features);
        // Then
        Map < String, Property<?>> properties = conf.getProperties();
        Assertions.assertNotNull(properties);
    }
    
    @Test
    public void testErrorHandler() throws SAXException {
        assertThrows(SAXParseException.class, () -> {
            XmlParserErrorHandler eh = new XmlParserErrorHandler();
            eh.warning(null);
            eh.fatalError(new SAXParseException("", null));
        });
    }
    
    @Test
    public void testErrorHandler2() throws SAXException {
        assertThrows(SAXParseException.class, () -> {
            XmlParserErrorHandler eh = new XmlParserErrorHandler();
            eh.warning(null);
            eh.error(new SAXParseException("", null));
        });
    }

}
