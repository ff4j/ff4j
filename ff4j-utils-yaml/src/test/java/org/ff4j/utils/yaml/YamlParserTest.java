package org.ff4j.utils.yaml;

import java.io.FileNotFoundException;

import org.ff4j.parser.FF4jConfigFile;
import org.junit.Assert;
import org.junit.jupiter.api.Test;

/**
 * Parsing sample files.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class YamlParserTest {
    
    @Test
    public void testParsing() throws FileNotFoundException {
        FF4jConfigFile configFile = new YamlParser().parse("test-ff4j-parser.yml");
        
        // roles
        Assert.assertNotNull(configFile.getRoles());
        Assert.assertEquals(3, configFile.getRoles().size());
        Assert.assertEquals(4, configFile.getRoles().get("USER").size());
        
        // users
        Assert.assertNotNull(configFile.getUsers());
        Assert.assertEquals(2, configFile.getUsers().size());
        Assert.assertEquals(1, configFile.getUsers().get("pierre").getRoles().size());
        
        // acl
        Assert.assertNotNull(configFile.getAcls());
        Assert.assertNotNull(configFile.getAcls().get("WEB_CONSOLE"));

        // Properties
        Assert.assertNotNull(configFile.getProperties());
        Assert.assertEquals(7, configFile.getProperties().size());
        Assert.assertNotNull(configFile.getProperties().get("a"));
        
        // Features
        Assert.assertNotNull(configFile.getFeatures());
        Assert.assertEquals(3, configFile.getFeatures().size());
        Assert.assertEquals(2, configFile.getFeatures().get("first").getToggleStrategies().size());
        
        System.out.println(configFile);
    }

}
