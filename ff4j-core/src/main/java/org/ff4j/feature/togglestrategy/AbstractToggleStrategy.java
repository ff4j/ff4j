package org.ff4j.feature.togglestrategy;

import java.util.function.Predicate;

import org.ff4j.property.evaluate.AbstractEvaluationPolicy;
import org.ff4j.property.evaluate.FF4jEvaluationContext;
import org.ff4j.backend.Backend;
import org.ff4j.feature.Flag;

/**
 * Super class for Toggle Strategy implementation with utilities.
 */
public abstract class AbstractToggleStrategy
        extends AbstractEvaluationPolicy<Boolean, Flag>
        implements Predicate<FF4jEvaluationContext> {

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
    public AbstractToggleStrategy(Backend backend, Flag relatedFeature, FF4jEvaluationContext config) {
        super(backend, relatedFeature, config);
    }

    /** {@inheritDoc} */
    @Override
    public Boolean evaluate(FF4jEvaluationContext context) {
        return test(context);
    }

}
