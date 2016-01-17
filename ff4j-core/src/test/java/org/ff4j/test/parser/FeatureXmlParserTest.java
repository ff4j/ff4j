package org.ff4j.test.parser;

/*
 * #%L
 * ff4j-core
 * $Id:$
 * $HeadURL:$
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
import org.junit.Assert;
import org.junit.Test;
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

        Assert.assertEquals(7, features.size());
        Assert.assertTrue(features.containsKey("f0"));
        Assert.assertNotNull(features.get("f0").getDescription());
        Assert.assertNotNull(features.get("f0").getPermissions());
        Assert.assertEquals(2, features.get("f0").getPermissions().size());
        Assert.assertNotNull(features.get("f0").getFlippingStrategy());
        Assert.assertEquals(1, features.get("f0").getFlippingStrategy().getInitParams().size());
        Assert.assertNotNull(features.get("f0").getGroup());
        Assert.assertEquals("group3", features.get("f0").getGroup());
        Assert.assertTrue(features.containsKey("f1"));
        Assert.assertTrue(features.containsKey("f2"));
        Assert.assertTrue(features.containsKey("f3"));
        Assert.assertTrue(features.containsKey("f4"));
        Assert.assertTrue(features.containsKey("f5"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testSaxException() {
        InputStream in = new ByteArrayInputStream("<TOTO>Invalid</TOTO2>".getBytes());
        new XmlParser().parseConfigurationFile(in);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testNullFile() {
        new XmlParser().parseConfigurationFile(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testLoaderRequiredUid() {
        InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-ko-uidrequired.xml");
        new XmlParser().parseConfigurationFile(in);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testLoaderRequiredEnable() {
        InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-ko-enablerequired.xml");
        new XmlParser().parseConfigurationFile(in);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testLoaderLoadInvalidStream() throws IOException {
        InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-ok.xml");
        in.close();
        new XmlParser().parseConfigurationFile(in);
    }

    @Test
    public void importThenExport() throws IOException {
        // Given
        XmlParser parser = new XmlParser();
        InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-import-export.xml");
        Map<String, Feature> features = parser.parseConfigurationFile(in).getFeatures();
        Assert.assertNotNull(features);
        // When
        InputStream in2 = parser.exportFeatures(features);
        // Then
        // output is OK
        Map<String, Feature> features2 = parser.parseConfigurationFile(in2).getFeatures();
        Assert.assertNotNull(features2);
        Assert.assertEquals(features.size(), features2.size());
    }
    
    @Test
    public void importThenExportFeatures2() throws IOException {
        // Given
        XmlParser parser = new XmlParser();
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-parser-all.xml");
        Map<String, Feature> features = parser.parseConfigurationFile(in).getFeatures();
        Assert.assertNotNull(features);
        // When
        InputStream in3 = parser.exportFeatures(features);
        // Then
        // output is OK
        Map<String, Feature> features2 = parser.parseConfigurationFile(in3).getFeatures();
        Assert.assertNotNull(features2);
        Assert.assertEquals(features.size(), features2.size());
    }
    
    @Test
    public void importThenExportALL() throws IOException {
        // Given
        XmlParser parser = new XmlParser();
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-parser-all.xml");
        XmlConfig conf = parser.parseConfigurationFile(in);
        Assert.assertNotNull(conf.getFeatures());
        Assert.assertNotNull(conf.getProperties());
        // When
        InputStream in3 = parser.exportAll(conf);
        // Then
        XmlConfig conf2 = parser.parseConfigurationFile(in3);
        Assert.assertNotNull(conf2.getFeatures());
        Assert.assertNotNull(conf2.getProperties());
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
        Assert.assertNotNull(features);
        Feature f = features.get("first");
        Assert.assertNotNull(f);
        Assert.assertNotNull(f.getUid());
        Assert.assertNotNull(f.getCustomProperties());
        Assert.assertNotNull(f.getCustomProperties().get("ppint"));
        Assert.assertEquals(f.getCustomProperties().get("ppint").asInt(), 12);
        Assert.assertEquals(f.getCustomProperties().get("ppdouble").asDouble(), 12.5,0);
        Assert.assertEquals(f.getCustomProperties().get("ppboolean").asBoolean(),true);
        Assert.assertEquals(f.getCustomProperties().get("ppstring").asString(), "hello");
        Assert.assertEquals(f.getCustomProperties().get("regionIdentifier").asString(), "AMER");
        Assert.assertNotNull(f.getCustomProperties().get("regionIdentifier").getFixedValues());
        Assert.assertFalse(f.getCustomProperties().get("regionIdentifier").getFixedValues().isEmpty());
        PropertyLogLevel pll = (PropertyLogLevel) f.getCustomProperties().get("myLogLevel");
        Assert.assertEquals(pll.getValue(), LogLevel.DEBUG);
        
        // Then
        Map < String, Property<?>> properties = conf.getProperties();
        Assert.assertNotNull(properties);
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
        Assert.assertNotNull(features);
        // Then
        Map < String, Property<?>> properties = conf.getProperties();
        Assert.assertNotNull(properties);
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
        Assert.assertNotNull(features);
        // Then
        Map < String, Property<?>> properties = conf.getProperties();
        Assert.assertNotNull(properties);
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
        Assert.assertNotNull(features);
        // Then
        Map < String, Property<?>> properties = conf.getProperties();
        Assert.assertNotNull(properties);
    }
    
    @Test(expected = SAXParseException.class)
    public void testErrorHandler() throws SAXException {
        XmlParserErrorHandler eh = new XmlParserErrorHandler();
        eh.warning(null);
        eh.fatalError(new SAXParseException("", null));
    }
    
    @Test(expected = SAXParseException.class)
    public void testErrorHandler2() throws SAXException {
        XmlParserErrorHandler eh = new XmlParserErrorHandler();
        eh.warning(null);
        eh.error(new SAXParseException("", null));
    }

}
