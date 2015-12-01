package org.ff4j.drools;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.junit.Test;

/**
 * Externalize the flipping strategy into
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jDroolsFlippingStrategyTest {
    
    @Test
    public void testX() {
        
        FF4j ff4j = new FF4j();
        
        Feature f1 = new Feature("f1", true);
        FF4jDroolsFlippingStrategy stra = new FF4jDroolsFlippingStrategy("ff4jDroolsStrategy2");
        f1.setFlippingStrategy(stra);
        ff4j.create(f1);
        ff4j.check("f1");
                
        
    }
}