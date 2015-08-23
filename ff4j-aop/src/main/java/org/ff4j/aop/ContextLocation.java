package org.ff4j.aop;

/**
 * Enumeration that represents the location of the flipping context execution
 * which will be passed to the flipping strategy.
 *
 * {@see Flip} annotation
 * {@see FlippingExecutionContext}
 */
public enum ContextLocation {
    /**
     * {@link Flip} will not use a {@link org.ff4j.core.FlippingExecutionContext} to switch the feature.
     */
    NONE,

    /**
     * {@link Flip} will use the first method parameter that is an instance of {@link org.ff4j.core.FlippingExecutionContext}.
     */
    PARAMETER,

    /**
     * {@link Flip} will use the {@link org.ff4j.FF4j} instance to retrieve the current context.
     */
    FF4J
}
