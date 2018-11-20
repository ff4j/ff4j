package org.ff4j.parser.yaml;

import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.parser.xml.XmlParserV1;
import org.ff4j.parser.xml.XmlParserV2;
import org.ff4j.parser.yaml.YamlParser;
import org.junit.jupiter.api.Test;

public class Parsers {
    
    @Test
    public void testParsers() {
        
        FF4jConfigFile xmlConfigV1 = new XmlParserV1().parse("test-ff4j-v1.xml");
        
        
        
        FF4jConfigFile xmlConfigV2 = new XmlParserV2().parse("test-ff4j-v2.xml");
        
        FF4jConfigFile yamlConfig = new YamlParser().parse("test-ff4j-parser.yml");
        
        
    }

}
