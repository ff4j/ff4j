package org.ff4j.test.parser;

import java.nio.file.Paths;

import org.ff4j.inmemory.parser.yml.YamlParser;
import org.junit.Test;

public class YamlParserTest {
    
    @Test
    public void test() throws Exception {
        testFile("yaml-01-dummy.yml");
        /*testFile("yaml-02-nolist.yml");
        testFile("yaml-03-onlySimpleList.yml");
        testFile("yaml-04-singleComplexList.yml");
        testFile("yaml-05-full.yml");
        testFile("kub1.yml");
        testFile("yaml-06-multilist-multilvl.yml");*/
        //testFile("yaml-07-severallist-samelevel.yml");
        //testFile("yaml-08-ff4j.yml");
    }
    
    private  void testFile(String fileName) throws Exception {
        System.out.println("DATA:" + 
                YamlParser.parseYamlFile(
                        Paths.get(YamlParser.class.getClassLoader()
                                .getResource(fileName).toURI()).toFile()));
    }
    
}
