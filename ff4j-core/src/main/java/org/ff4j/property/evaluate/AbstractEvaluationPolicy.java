package org.ff4j.property.evaluate;

import org.ff4j.backend.BackendSupport;
import org.ff4j.property.Property;

/**
 * Compute treatment to get dynamic value.
 *
 * @param <T>
 *          manipulated object
 */
public abstract class AbstractEvaluationPolicy<T, P extends Property<T>> implements FF4jEvaluationPolicy<T> {

    /** Access all objects in the repository, security, audit. */
    private final BackendSupport backend;

    /** Hold reference to parent property. */
    private final P target;

    /** Configuration for the evaluation policy. */
    private final FF4jEvaluationContext config;

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
    public AbstractEvaluationPolicy(BackendSupport backend, P relatedProperty, FF4jEvaluationContext config) {
        this.config          = config;
        this.backend         = backend;
        this.target = relatedProperty;
    }

    /**
     * Gets backend
     *
     * @return value of backend
     */
    public BackendSupport getBackend() {
        return backend;
    }

    /**
     * Gets target
     *
     * @return value of target
     */
    public P getTarget() {
        return target;
    }

    /**
     * Gets config
     *
     * @return value of config
     */
    public FF4jEvaluationContext getConfig() {
        return config;
    }
}
