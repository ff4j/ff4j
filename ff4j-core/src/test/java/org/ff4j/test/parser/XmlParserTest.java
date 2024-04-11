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

import java.io.ByteArrayInputStream;


import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;

import org.ff4j.conf.XmlConfig;
import org.ff4j.conf.XmlParser;
import org.ff4j.property.Property;
import org.ff4j.test.unsafe.UnsafeFlippingStrategy;
import org.ff4j.test.unsafe.UnsafeProperty;
import org.junit.Assert;
import org.junit.Test;

public class XmlParserTest {
    
    private XmlConfig parseFile(String fileName) {
        // Given
        InputStream in = getClass().getClassLoader().getResourceAsStream(fileName);
        if (in == null) Assert.fail("Xml file must exist");
        // When
        return new XmlParser().parseConfigurationFile(in);
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
    
    @Test
    public void testParsingPropertiesUnsafe() throws IOException {
        Assert.assertThrows(IllegalArgumentException.class, () -> {
            parseFile("test-property6.xml");
        });
        Assert.assertEquals(0, UnsafeProperty.count);
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
    public void testParsingFeatureInvalidStrategy() throws IOException {
        Assert.assertThrows(IllegalArgumentException.class, () -> {
            parseFile("test-feature-ko8.xml");
        });
        Assert.assertEquals(0, UnsafeFlippingStrategy.count);
    }
    
    @Test
    public void testParseXMLWithSpecialCharacters() throws IOException {
        // Given a config file with Special Characters in XML
        XmlConfig xmlConfig1 = parseFile("test-parser-specialchars.xml");
        // When parsed the special characters are replaced by expected values 
        Assert.assertEquals("description \"&>OK<'", xmlConfig1
                .getFeatures().get("first").getDescription());
        // When parsed values protected by CDATA to interpret special chars
        Assert.assertTrue(xmlConfig1
                .getFeatures().get("first")
                .getCustomProperties().get("prop2").getFixedValues()
                .contains("https://en.wikipedia.org/w/index.php?title=XML&action=edit&section=4"));
        
        // Given XmlConfig with special char
        ByteArrayInputStream bais = (ByteArrayInputStream) new XmlParser().exportAll(xmlConfig1);
        // When converting back to XML data
        int n = bais.available();
        byte[] bytes = new byte[n];
        bais.read(bytes, 0, n);
        String s = new String(bytes, StandardCharsets.UTF_8);
        // Then special characters are escaped and values without quotes are protected with CDATA.
        Assert.assertTrue(s.contains("description &quot;&amp;&gt;OK&lt;&apos;"));
        Assert.assertTrue(s.contains("<![CDATA[https://en.wikipedia.org/w/index.php?title=XML&action=edit&section=4]]>"));
        // When parsing back to XML
        InputStream is = new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8));
        XmlConfig xmlConfig2 = new XmlParser().parseConfigurationFile(is);
        // Then I get back the same values
        Assert.assertEquals(
           xmlConfig2.getFeatures().get("first").getDescription(), 
           xmlConfig1.getFeatures().get("first").getDescription());
    }
    
    @Test
    public void testNullValues() throws IOException {
        new XmlParser().exportProperties(new HashMap<String, Property<?>>());
    }

}
