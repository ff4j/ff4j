package org.ff4j.strategy;

/**
 * Randomly activate/desactivate feature
 * 
 * @author clunven
 */
public class RandomFlipStrategy implements FlippingStrategy {

    /** Return equiprobability as 50%. */
    private static final double HALF = 0.5;

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, String initValue) {
        // Nothing to do
    }

    /** {@inheritDoc} */
    @Override
    public boolean activate(String featureName, Object... executionContext) {
        return Math.random() > HALF;
    }

}
