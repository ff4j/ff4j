package org.ff4j.test;

import static org.junit.Assert.fail;
import junit.framework.Assert;

import org.ff4j.Feature;
import org.ff4j.Flipper;
import org.junit.Test;


/**
 * Unit Testing
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FlipperTest {
	
	@Test
	public void test_base_operations() {
		
		// Initialization (Inmemory)
		Flipper.createFeature(new Feature("tremendousFeature", true, "it will rocks"));
		
		Assert.assertNotNull(Flipper.getFeature("tremendousFeature"));
		Assert.assertTrue(Flipper.getFeature("tremendousFeature").isEnable());
		Assert.assertTrue(Flipper.getFeature("tremendousFeature").getDescription().startsWith("it will"));
		
		// Test init value
		if (!Flipper.isFlipped("tremendousFeature")) {
			fail();
		}
		
		// Update
		Flipper.disableFeature("tremendousFeature");
		if (Flipper.isFlipped("tremendousFeature")) {
			fail();
		}
		
		//Update
		Flipper.enableFeature("tremendousFeature");
		if (!Flipper.isFlipped("tremendousFeature")) {
			fail();
		}
		
		// if you get here that'k ok
	}
	
}
