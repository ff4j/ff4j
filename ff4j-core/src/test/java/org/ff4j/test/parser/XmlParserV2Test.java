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
import org.ff4j.parser.xml.XmlParserV1;
import org.ff4j.parser.xml.XmlParserV2;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.ff4j.test.FF4jTestDataSet;
import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Testing XMLParser for v2 configuration files")
public class XmlParserV2Test implements FF4jTestDataSet {
  
    /** DataSet. **/
    protected FF4jConfigFile testDataSet;
    
    /** {@inheritDoc} */
    @BeforeEach
    public void setUp() throws Exception {
        testDataSet = expectConfig();
    }
    
    @Test
    @DisplayName("Parsing xml v1 files, should load features, groups")
    public void testLoaderXMLFile() {
        // Given
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-testDataset.xml");
        Map<String, Feature> features = new XmlParserV2().parse(in).getFeatures();
        // Then
        Assert.assertTrue(features.containsKey(F1));
        Assert.assertTrue(features.containsKey(F2));
        Assert.assertTrue(features.containsKey(F3));
        Assert.assertTrue(features.containsKey(F4));
        Assertions.assertEquals(testDataSet.getFeatures().size(), features.size());
        Feature f4 = features.get(F4);
        Assertions.assertEquals(F4, f4.getUid());
        Assertions.assertTrue(f4.getDescription().isPresent() && !"".equals(f4.getDescription().get()));
    }
    
    @Test
    @DisplayName("Xml Parser should failed on invalid XML document")
    public void testInvalidDXmlDocument() {
       assertThrows(IllegalArgumentException.class, () -> {
           new XmlParserV2().parse(new ByteArrayInputStream("<TOTO>Invalid</TOTO2>".getBytes()));
       });
    }
    
    @Test
    @DisplayName("Xml Parser should failed on null InputStream")
    public void testNullInputStream() {
       assertThrows(IllegalArgumentException.class, () -> {
           new XmlParserV2().parse((InputStream) null);
       });
    }
    
    @Test
    @DisplayName("uid attribute is required for features")
    public void testLoaderRequiredUid() {
       assertThrows(IllegalArgumentException.class, () -> {
         new XmlParserV2().parse(
                 getClass().getClassLoader()
                 .getResourceAsStream("testXmlParserV1-ko-uidrequired.xml"));
       });
    }
    
    @Test
    @DisplayName("enable attribute is required for features")
    public void testLoaderRequiredEnable() {
       assertThrows(IllegalArgumentException.class, () -> {
         new XmlParserV2().parse(
                 getClass().getClassLoader()
                 .getResourceAsStream("testXmlParserV1-ko-enablerequired.xml"));
       });
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
        XmlParserV2 parser = new XmlParserV2();
        InputStream in = getClass().getClassLoader().getResourceAsStream("v2/testXmlParserV2-full.xml");
        // When
        FF4jConfigFile conf = parser.parse(in);
        // Then
        Map<String, Feature> features = conf.getFeatures();
        Assert.assertNotNull(features);
        Feature f = features.get(F2);
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
        new XmlParserV2().parse(in);
    }
    
    @Test
    @DisplayName("Invalid fixed Value in Properties")
    public void testParsingProperties() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-property1.xml"));
    }
    
    @Test
    @DisplayName("Propertymust have a name")
    public void testParsingProperties2NoName() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-property2.xml"));
    }
    
    @Test
    @DisplayName("Property must have a value")
    public void testParsingProperties3NoValue() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-property3.xml"));
    }
    
    @Test
    @DisplayName("Property must have a proper type")
    public void testParsingPropertiesInvalidType() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-property4.xml"));
    }
    
    @Test
    @DisplayName("Property tag must be unique")
    public void testParsingPropertiesMoreTag() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-property5.xml"));
    }
    
    @Test
    @DisplayName("Feature class name is required")
    public void testParsingFeatureNoClass() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-feature-ko1.xml"));
    }
    
    @Test
    @DisplayName("Feature value name is require")
    public void testParsingFeatureNoValue() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-feature-ko2.xml"));
    }
    
    @Test
    @DisplayName("Feature tag must be unique")
    public void testParsingFeatureInvalidTag() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-feature-ko3.xml"));
    }
    
    @Test
    @DisplayName("Feature is the only allowed tag")
    public void testParsingFeatureInvalidTag2() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-feature-ko4.xml"));
    }
    
    @Test
    @DisplayName("Feature group tag are above features")
    public void testParsingFeatureInvalidTag5() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-feature-ko5.xml"));
    }
    
    @Test
    @DisplayName("Feature name must be uniqeu")
    public void testParsingFeatureInvalidTag4() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-feature-ko6.xml"));
    }
    
    @Test
    @DisplayName("Feature Toggle strategy param must have value")
    public void testParsingFeatureInvalidTag7() throws IOException {
        assertThrows(IllegalArgumentException.class, () -> parseFile("v2/testXmlParserV2-feature-ko7.xml"));
    }    

}
