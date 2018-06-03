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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.parser.xml.XmlParser;
import org.ff4j.parser.xml.XmlParserErrorHandler;
import org.junit.Test;
//import org.junit.jupiter.api.DisplayName;
//import org.junit.jupiter.params.ParameterizedTest;
//import org.junit.jupiter.params.provider.ValueSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

//@DisplayName("Testing utility class to parse data from XML.")
public class XmlParserTestJunit5 {
  
    // -------------------------------------------------------------------------
    // ------------------- LIMIT USE CASES       -------------------------------
    // -------------------------------------------------------------------------
    
    @Test(expected = IllegalArgumentException.class)
    public void testSaxException() {
        new XmlParser().parse(new ByteArrayInputStream("<TOTO>Invalid</TOTO2>".getBytes()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testNullFile() {
        new XmlParser().parse((InputStream) null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testNullInputStream() {
        new XmlParser().parse((String) null);
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
    
    // -------------------------------------------------------------------------
    // ------------------- ROLES                 -------------------------------
    // -------------------------------------------------------------------------
    
    //@ParameterizedTest
    //@ValueSource(strings = { "ff4j-v2.xml" })
    public void testParseXML(String xmlFileName) {
        // Given (fileName)
        // When        
        FF4jConfigFile xmlData = new XmlParser().parse(xmlFileName);
        
        // Then (Roles)
        assertThat(xmlData.getRoles().size(), is(equalTo(3)));
        assertNotNull(xmlData.getRoles().get("SUPERUSER"));
        assertTrue(xmlData.getRoles().get("SUPERUSER").contains("VIEW_AUDITTRAIL"));

        // Then (Users)
        System.out.println(xmlData.getUsers().values().iterator().next().toString());
    }
    
    
}
