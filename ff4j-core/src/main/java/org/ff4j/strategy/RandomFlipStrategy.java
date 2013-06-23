package org.ff4j.strategy;


import org.ff4j.Flipper;

/**
 * Randomly activate/desactivate feature
 * @author clunven
 */
public class RandomFlipStrategy implements FlippingStrategy {

	/** {@inheritDoc} */
	public boolean activate(String featureName, Object... executionContext) {
		return Flipper.getStore().read(featureName).isEnabled() && (Math.random() > 0.5);
	}

}
