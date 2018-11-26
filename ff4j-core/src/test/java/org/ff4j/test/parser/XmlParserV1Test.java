package org.ff4j.test.parser;

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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import org.ff4j.feature.Feature;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.parser.xml.XmlParserErrorHandler;
import org.ff4j.parser.xml.XmlParserV1;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.junit.Assert;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.xml.sax.SAXParseException;

@DisplayName("Testing XMLParser for v1 configuration files")
public class XmlParserV1Test {
  
    @Test
    @DisplayName("Parsing xml v1 files, should load features, groups")
    public void testLoaderXMLFile() {
        InputStream in = getClass().getClassLoader().getResourceAsStream("v1/testXmlParserV1-full-features.xml");
        Map<String, Feature> features = new XmlParserV1().parse(in).getFeatures();
        Assert.assertEquals(7, features.size());
        Assert.assertTrue(features.containsKey("f0"));
        Assert.assertNotNull(features.get("f0").getDescription());
        Assert.assertNotNull(features.get("f0").getToggleStrategies().get(0));
        Assert.assertEquals(1, features.get("f0").getToggleStrategies().get(0).getParams().size());
        Assert.assertTrue(features.get("f0").getGroup().isPresent());
        Assert.assertEquals("group3", features.get("f0").getGroup().get());
        Assert.assertTrue(features.containsKey("f1"));
        Assert.assertTrue(features.containsKey("f2"));
        Assert.assertTrue(features.containsKey("f3"));
        Assert.assertTrue(features.containsKey("f4"));
        Assert.assertTrue(features.containsKey("f5"));
    }
    
    @Test
    @DisplayName("Xml Parser should failed on invalid XML document")
    public void testInvalidDXmlDocument() {
       assertThrows(IllegalArgumentException.class, () -> {
           new XmlParserV1().parse(new ByteArrayInputStream("<TOTO>Invalid</TOTO2>".getBytes()));
       });
    }
    
    @Test
    @DisplayName("Xml Parser should failed on null InputStream")
    public void testNullInputStream() {
       assertThrows(IllegalArgumentException.class, () -> {
           new XmlParserV1().parse((InputStream) null);
       });
    }
    
    @Test
    @DisplayName("Error Handler should raised SAXParseException if invalid format")
    public void testErrorHandlerFatalError() {
       assertThrows(SAXParseException.class, () -> {
           XmlParserErrorHandler eh = new XmlParserErrorHandler();
           eh.warning(null); 
           eh.fatalError(new SAXParseException("", null));
       });
    }
    
    @Test
    @DisplayName("Error Handler should raised SAXParseException if invalid format")
    public void testErrorHandler() {
       assertThrows(SAXParseException.class, () -> {
           XmlParserErrorHandler eh = new XmlParserErrorHandler();
           eh.warning(null); 
           eh.error(new SAXParseException("", null));
       });
    }
    
    @Test
    @DisplayName("uid attribute is required for features")
    public void testLoaderRequiredUid() {
       assertThrows(IllegalArgumentException.class, () -> {
         new XmlParserV1().parse(
                 getClass().getClassLoader()
                 .getResourceAsStream("v1/testXmlParserV1-ko-uidrequired.xml"));
       });
    }
    
    @Test
    @DisplayName("enable attribute is required for features")
    public void testLoaderRequiredEnable() {
       assertThrows(IllegalArgumentException.class, () -> {
         new XmlParserV1().parse(
                 getClass().getClassLoader()
                 .getResourceAsStream("v1/testXmlParserV1-ko-enablerequired.xml"));
       });
    }
    
    @Test
    @DisplayName("Import features from XML export and count")
    public void importThenExportFeatures() throws IOException {
        // Given
        XmlParserV1 parser = new XmlParserV1();
        InputStream in = getClass().getClassLoader().getResourceAsStream("v1/testXmlParserV1-import-export.xml");
        Map<String, Feature> features = parser.parse(in).getFeatures();
        Assert.assertNotNull(features);
        // When
        InputStream in2 = XmlParserV1.exportFeatures(features.values().stream());
        Map<String, Feature> features2 = parser.parse(in2).getFeatures();
        Assert.assertNotNull(features2);
        Assert.assertEquals(features.size(), features2.size());
    }
    
    @Test
    @DisplayName("Import All from XML export and count")
    public void importThenExportALL() throws IOException {
        // Given
        XmlParserV1 parser = new XmlParserV1();
        InputStream in = getClass().getClassLoader().getResourceAsStream("v1/testXmlParserV1-full.xml");
        FF4jConfigFile conf = parser.parse(in);
        Assert.assertNotNull(conf.getFeatures());
        Assert.assertNotNull(conf.getProperties());
        // When
        InputStream in3 = XmlParserV1.exportAll(conf);
        // Then
        FF4jConfigFile conf2 = parser.parse(in3);
        Assert.assertNotNull(conf2.getFeatures());
        Assert.assertNotNull(conf2.getProperties());
    }
    
    @Test
    @DisplayName("Parse Custom Properties for a feature")
    public void testPropertiesParsing() throws IOException {
        // Given
        XmlParserV1 parser = new XmlParserV1();
        InputStream in = getClass().getClassLoader().getResourceAsStream("v1/testXmlParserV1-full.xml");
        // When
        FF4jConfigFile conf = parser.parse(in);
        // Then
        Map<String, Feature> features = conf.getFeatures();
        Assert.assertNotNull(features);
        Feature f = features.get("first");
        Assert.assertNotNull(f);
        Assert.assertNotNull(f.getUid());
        Assert.assertNotNull(f.getProperties());
        Assert.assertNotNull(f.getProperties().get("ppint"));
        Assert.assertEquals(f.getProperties().get("ppint").asInt(), 12);
        Assert.assertEquals(f.getProperties().get("ppdouble").asDouble(), 12.5,0);
        Assert.assertEquals(f.getProperties().get("ppboolean").asBoolean(),true);
        Assert.assertEquals(f.getProperties().get("ppstring").asString(), "hello");
        Assert.assertEquals(f.getProperties().get("regionIdentifier").asString(), "AMER");
        Assert.assertTrue(f.getProperties().get("regionIdentifier").getFixedValues().isPresent());
        PropertyLogLevel pll = (PropertyLogLevel) f.getProperties().get("myLogLevel");
        Assert.assertEquals(pll.getValue(), LogLevel.DEBUG);
        // Then
        Map < String, Property<?>> properties = conf.getProperties();
        Assert.assertNotNull(properties);
    }
    
    private void parseFile(String fileName) {
        // Given
        InputStream in = getClass().getClassLoader().getResourceAsStream(fileName);
        if (in == null) Assert.fail("Xml file must exist");
        // When
        new XmlParserV1().parse(in);
    }
    
    @Test
    @DisplayName("Invalid fixed Value in Properties")
    public void testParsingProperties() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-property1.xml"));
    }
    
    @Test
    @DisplayName("Propertymust have a name")
    public void testParsingProperties2NoName() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-property2.xml"));
    }
    
    @Test
    @DisplayName("Property must have a value")
    public void testParsingProperties3NoValue() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-property3.xml"));
    }
    
    @Test
    @DisplayName("Property must have a proper type")
    public void testParsingPropertiesInvalidType() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-property4.xml"));
    }
    
    @Test
    @DisplayName("Property tag must be unique")
    public void testParsingPropertiesMoreTag() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-property5.xml"));
    }
    
    @Test
    @DisplayName("Feature class name is required")
    public void testParsingFeatureNoClass() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-feature-ko1.xml"));
    }
    
    @Test
    @DisplayName("Feature value name is require")
    public void testParsingFeatureNoValue() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-feature-ko2.xml"));
    }
    
    @Test
    @DisplayName("Feature tag must be unique")
    public void testParsingFeatureInvalidTag() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-feature-ko3.xml"));
    }
    
    @Test
    @DisplayName("Feature is the only allowed tag")
    public void testParsingFeatureInvalidTag2() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-feature-ko4.xml"));
    }
    
    @Test
    @DisplayName("Feature group tag are above features")
    public void testParsingFeatureInvalidTag5() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-feature-ko5.xml"));
    }
    
    @Test
    @DisplayName("Feature name must be unique")
    public void testParsingFeatureInvalidTag4() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-feature-ko6.xml"));
    }
    
    @Test
    @DisplayName("Feature Toggle strategy param must have value")
    public void testParsingFeatureInvalidTag7() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v1/testXmlParserV1-feature-ko7.xml"));
    }    

}
