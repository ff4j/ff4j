package org.ff4j.drools;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.junit.Assert;
import org.junit.Test;

/**
 * Externalize the flipping strategy into
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jDroolsKBaseProgrammatic {
    
    @Test
    public void testDroolsStrategyFromkBaseName() {
        // Given
        FF4j ff4j = new FF4j();
        // When
        Feature f1 = new Feature("f1", true);
        f1.setFlippingStrategy(new FF4jDroolsFlippingStrategy("ff4jDroolsStrategy"));
        ff4j.createFeature(f1);
        Assert.assertTrue(ff4j.exist("f1"));
        // Then
        Assert.assertTrue(ff4j.check("f1"));
    }
        
}