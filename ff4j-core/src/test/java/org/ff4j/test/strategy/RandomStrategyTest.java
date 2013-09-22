package org.ff4j.test.strategy;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.RandomFlipStrategy;
import org.ff4j.test.DefaultAuthorisationManager;
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
		FF4j f4 = new FF4j(new InMemoryFeatureStore());
		f4.setAutocreate(true);
		FF4j.sCreateFeature(new Feature("default", true, "desc", null, new RandomFlipStrategy()));
		int nbOK = 0;
		int nbKO = 0;
		for(int i=0;i<1000;i++) {
			if (FF4j.sIsFlipped("default")) {
				nbOK++;
			} else {
				nbKO++;
			}
		}
		Assert.assertTrue("both result occured", nbOK > 0 && nbKO > 0);
	}

}
