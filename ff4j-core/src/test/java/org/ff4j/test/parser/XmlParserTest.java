package org.ff4j.test.parser;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;

import org.ff4j.conf.XmlParser;
import org.ff4j.property.Property;
import org.junit.Assert;
import org.junit.Test;

public class XmlParserTest {
    
    private void parseFile(String fileName) {
        // Given
        InputStream in = getClass().getClassLoader().getResourceAsStream(fileName);
        if (in == null) Assert.fail("Xml file must exist");
        // When
        new XmlParser().parseConfigurationFile(in);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingProperties() throws IOException {
       parseFile("test-property1.xml");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingProperties2NoName() throws IOException {
        parseFile("test-property2.xml");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingProperties3NoValue() throws IOException {
        parseFile("test-property3.xml");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingPropertiesInvalidType() throws IOException {
        parseFile("test-property4.xml");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingPropertiesMoreTag() throws IOException {
        parseFile("test-property5.xml");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingFeatureNoClass() throws IOException {
        parseFile("test-feature-ko1.xml");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingFeatureNoValue() throws IOException {
        parseFile("test-feature-ko2.xml");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingFeatureInvalidTag() throws IOException {
        parseFile("test-feature-ko3.xml");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingFeatureInvalidTag2() throws IOException {
        parseFile("test-feature-ko4.xml");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingFeatureInvalidTag3() throws IOException {
        parseFile("test-feature-ko5.xml");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingFeatureInvalidTag4() throws IOException {
        parseFile("test-feature-ko6.xml");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParsingFeatureInvalidTag5() throws IOException {
        parseFile("test-feature-ko7.xml");
    }
    
    @Test
    public void testNullValues() throws IOException {
        new XmlParser().escapeXML(null);
        new XmlParser().exportProperties(new HashMap<String, Property<?>>());
    }

}
