package org.ff4j.test.parser;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;

import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.parser.xml.XmlParser;
import org.ff4j.parser.xml.XmlParserErrorHandler;
import org.junit.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

@DisplayName("Testing utility class to parse data from XML.")
public class XmlParserTestJunit5 {
  
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
    
    @ParameterizedTest
    @ValueSource(strings = { "ff4j-v2.xml" })
    public void testParseXML(String xmlFileName) {
        // Given (fileName)
        // When        
        FF4jConfigFile xmlData = XmlParser.parseFile(xmlFileName);
        
        // Then (Roles)
        assertThat(xmlData.getRoles().size(), is(equalTo(3)));
        assertNotNull(xmlData.getRoles().get("SUPERUSER"));
        assertTrue(xmlData.getRoles().get("SUPERUSER").contains("VIEW_AUDITTRAIL"));

        // Then (Users)
        System.out.println(xmlData.getUsers().values().iterator().next().toString());
    }
    
    
}
