package org.ff4j.strategy;

/**
 * Randomly activate/desactivate feature
 * @author clunven
 */
public class RandomFlipStrategy implements FlippingStrategy {

	/** {@inheritDoc} */
	public boolean activate(String featureName, Object... executionContext) {
		return Math.random() > 0.5;
	}

	/** {@inheritDoc} */
	public void init(String featureName, String initValue) {
		// Nothing to do
	}

}
