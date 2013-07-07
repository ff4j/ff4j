package org.ff4j.test.strategy;

import static org.ff4j.FF4j.enableFeature;
import static org.ff4j.FF4j.isFlipped;
import junit.framework.TestCase;

import org.ff4j.FF4j;
import org.junit.Assert;
import org.junit.Test;


/**
 * Unit Testing
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ExpressionFlipingStategyTest extends TestCase {
	
	@Test
	public void testExpression() throws Exception {
		
		new FF4j("ff4j-el.xml");
		Assert.assertTrue(isFlipped("D"));
		
		enableFeature("C");
		Assert.assertFalse(isFlipped("D"));
		
		enableFeature("B");
		Assert.assertTrue(isFlipped("D"));
		
	}
		

}
