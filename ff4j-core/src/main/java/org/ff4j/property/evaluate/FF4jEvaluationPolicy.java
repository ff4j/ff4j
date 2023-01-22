package org.ff4j.property.evaluate;

import org.ff4j.backend.BackendSupport;
import org.ff4j.exception.InvalidEvaluationPolicyException;
import org.ff4j.property.Property;
import org.ff4j.utils.Assert;

import java.util.Map;

/**
 * Compute treatment to get dynamic value.
 *
 * @param <T>
 *          manipulated object
 */
@FunctionalInterface
public interface FF4jEvaluationPolicy<T> {

   /**
     * Evaluation of the property value.
     *
     * @param evaluationContext
     *      inputs to help for evaluation
     * @return
     *      property value base on custom implementation
     */
    T evaluate(FF4jEvaluationContext evaluationContext);

    /**
     * Instantiate flipping strategy from its class name.
     *
     * @param className
     *      class name read from the persistence
     * @param config
     *      configuration as read in the persistence
     * @param backend
     *      backend to access context
     * @param relatedProperty
     *      reference property
     * @param <T>
     *      dynamic property type
     * @return
     *      evaluation property
     */
    static <T, P extends Property<T>> FF4jEvaluationPolicy<T> getEvaluationPolicy(
            String className, BackendSupport backend, P relatedProperty, FF4jEvaluationContext config) {
        try {
            Assert.assertNotNull(className);
            return (FF4jEvaluationPolicy<T>) Class.forName(className)
                 .getConstructor(BackendSupport.class, Property.class, Map.class)
                 .newInstance(backend, relatedProperty, config);
        } catch (Exception ie) {
            throw new InvalidEvaluationPolicyException(className, ie);
        }
    }

    
}
