package org.ff4j.drools;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.utils.Util;
import org.junit.Test;

/**
 * Externalize the flipping strategy into
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jDroolsDRLFiles {
   
    @Test
    public void testDroolsStrategyFromDRLFiles() {
        FF4j ff4j = new FF4j();
        Feature f1 = new Feature("f1", true);
        f1.setFlippingStrategy(new FF4jDroolsFlippingStrategy(Util.set("ff4jDroolsSample.drl")));
        ff4j.create(f1);
        ff4j.check("f1");
    }
    
}