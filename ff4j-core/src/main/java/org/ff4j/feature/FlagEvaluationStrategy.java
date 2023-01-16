package org.ff4j.feature;

import org.ff4j.FF4jContext;
import org.ff4j.backend.Backend;
import org.ff4j.feature.togglestrategy.AbstractToggleStrategy;
import org.ff4j.property.evaluate.AbstractEvaluationPolicy;
import org.ff4j.property.evaluate.FF4jEvaluationContext;

import java.util.Iterator;

/**
 * Override default evaluation to bring only the value.
 */
public class FlagEvaluationStrategy extends AbstractEvaluationPolicy<Boolean, Flag> {

    /**
     * Full Constructor.
     *
     * @param backend
     *      current backend
     * @param relatedProperty
     *      relatedProperty
     * @param config
     *      configuration
     */
    public FlagEvaluationStrategy(Backend backend, Flag relatedProperty, FF4jEvaluationContext config) {
        super(backend, relatedProperty, config);
    }

    /** {@inheritDoc} */
    @Override
    public Boolean evaluate(FF4jEvaluationContext evaluationContext) {
        // If the feature is not toggled value is false
        Flag flag = getTarget();
        boolean returned = flag.isToggled();
        if (returned && !flag.getToggleStrategies().isEmpty()) {
            Iterator<AbstractToggleStrategy> iterator = flag.getToggleStrategies().iterator();
            // Break as soon as one of the strategy return false
            while (returned && iterator.hasNext()) {
                returned = iterator.next().test(evaluationContext);
            }
        }
        return returned;
    }

}
