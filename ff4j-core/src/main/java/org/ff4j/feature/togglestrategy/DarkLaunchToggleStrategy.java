package org.ff4j.feature.togglestrategy;

import org.ff4j.backend.Backend;
import org.ff4j.feature.Flag;
import org.ff4j.property.PropertyDouble;
import org.ff4j.property.evaluate.FF4jEvaluationContext;

/**
 * Enable a toggle strategy for some user ratio.
 */
public class DarkLaunchToggleStrategy extends AbstractToggleStrategy {
    
    /** Params. */
    public static final String PARAM_WEIGHT = "weight";

    /**
     * Constructor with everything needed
     *
     * @param backend
     *      current backend
     * @param relatedFeature
     *      current feature
     * @param config
     *      configuration
     */
    public DarkLaunchToggleStrategy(Backend backend, Flag relatedFeature, FF4jEvaluationContext config) {
        this(backend, relatedFeature, (Double) config.getProperty(PARAM_WEIGHT).getValue());
    }

    /**
     * Constructor with everything needed
     *
     * @param backend
     *      current backend
     * @param relatedFeature
     *      current feature
     * @param weight
     *      weight
     */
    public DarkLaunchToggleStrategy(Backend backend, Flag relatedFeature, Double weight) {
        super(backend, relatedFeature, new FF4jEvaluationContext(new PropertyDouble(PARAM_WEIGHT, weight)));
        if (weight < 0 || weight > 1) throw new IllegalArgumentException("Percentage should be between 0 and 1");
    }

    /** {@inheritDoc} */
    @Override
    public boolean test(FF4jEvaluationContext ff4jEvaluationContext) {
        return Math.random() <= (Double) getConfig().getProperty(PARAM_WEIGHT).getValue();
    }
}
