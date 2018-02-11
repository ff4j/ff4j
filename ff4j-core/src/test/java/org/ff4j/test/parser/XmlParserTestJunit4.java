package org.ff4j.test.parser;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.util.Arrays;
import java.util.Collection;

import org.ff4j.inmemory.parser.FF4jConfigFile;
import org.ff4j.inmemory.parser.xml.XmlParser;
import org.ff4j.inmemory.parser.xml.XmlParserErrorHandler;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * Testing utility class to parse data from XML.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@RunWith(Parameterized.class)
public class XmlParserTestJunit4 {
  
    /** current file. */
    private String xmlFileName = null;
    
    public XmlParserTestJunit4(String xmlFileName) {
        this.xmlFileName = xmlFileName;
    }
    
    @Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {  
            {"ff4j-v2.xml"}  
           });
    }
    
    // -------------------------------------------------------------------------
    // ------------------- LIMIT USE CASES       -------------------------------
    // -------------------------------------------------------------------------
    
    @Test(expected = IllegalArgumentException.class)
    public void testSaxException() {
        XmlParser.parseInputStream(new ByteArrayInputStream("<TOTO>Invalid</TOTO2>".getBytes()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testNullFile() {
        XmlParser.parseFile(null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testNullInputStream() {
        XmlParser.parseInputStream(null);
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
    
    @Test
    public void testParseXML() {
        // Given (fileName)
        // When        
        FF4jConfigFile xmlData = XmlParser.parseFile(xmlFileName);
        
        // Then (Roles)
        assertEquals(3, xmlData.getRoles().size());
        assertNotNull(xmlData.getRoles().get("SUPERUSER"));
        assertTrue(xmlData.getRoles().get("SUPERUSER").contains("VIEW_AUDITTRAIL"));

        // Then (Users)
        
        
    }
    
    
}
