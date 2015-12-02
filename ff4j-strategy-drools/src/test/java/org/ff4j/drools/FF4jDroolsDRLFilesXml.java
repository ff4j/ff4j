package org.ff4j.drools;

import org.ff4j.FF4j;
import org.junit.Assert;
import org.junit.Test;

/**
 * Externalize the flipping strategy into
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jDroolsDRLFilesXml {
   
    @Test
    public void testDroolsStrategyDRLFiles() {
        // Given
        FF4j ff4j = new FF4j("ff4j-drl.xml");
        // When
        Assert.assertTrue(ff4j.exist("f1"));
        // Then
        Assert.assertTrue(ff4j.check("f1"));
    }
    
}