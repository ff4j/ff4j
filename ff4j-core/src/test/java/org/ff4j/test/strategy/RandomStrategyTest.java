package org.ff4j.test.strategy;

import junit.framework.Assert;

import org.ff4j.Feature;
import org.ff4j.Flipper;
import org.ff4j.strategy.RandomFlipStrategy;
import org.junit.Test;

/**
 * Unit Testing
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class RandomStrategyTest {
	
	/**
	 * On chance over 2 to the power of 1000 that this test failed but
	 * assuming on 1000 tries should be both OK and KO.
	 */
	@Test
	public void testRandomStrategy() {
		Flipper.createFeature(new Feature("default", true, "desc", null, new RandomFlipStrategy()));
		int nbOK = 0;
		int nbKO = 0;
		for(int i=0;i<1000;i++) {
			if (Flipper.isFlipped("default")) {
				nbOK++;
			} else {
				nbKO++;
			}
		}
		Assert.assertTrue("both result occured", nbOK > 0 && nbKO > 0);
	}

}
