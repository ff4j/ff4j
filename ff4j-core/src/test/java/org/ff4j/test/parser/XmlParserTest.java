package org.ff4j.test.parser;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.ff4j.parser.xml.XmlParser;
import org.ff4j.parser.xml.XmlParserErrorHandler;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.xml.sax.SAXParseException;

@DisplayName("XML Parser Tests.")
public class XmlParserTest {
  
    @Test
    @DisplayName("Xml Parser should failed on invalid XML document")
    public void testInvalidDXmlDocument() {
       assertThrows(IllegalArgumentException.class, () -> {
           new XmlParser().parse(new ByteArrayInputStream("<TOTO>Invalid</TOTO2>".getBytes()));
       });
    }
    
    @Test
    @DisplayName("Xml Parser should failed on null InputStream")
    public void testNullInputStream() {
       assertThrows(IllegalArgumentException.class, () -> {
           new XmlParser().parse((InputStream) null);
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

}
