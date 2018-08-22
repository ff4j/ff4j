package org.ff4j.feature.strategy;

import java.util.function.Predicate;

import org.ff4j.FF4jContext;
import org.ff4j.feature.Feature;

/**
 * Wrapper for {@link TogglePredicate} to implement {@link Predicate} and ease evolutions.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class ToggleContext extends FF4jContext {
   
    /** Current feature to be evaluated. */
    private Feature feature;
    
    /**
     * Allows default (reflection)
     */
    public ToggleContext(Feature feature, FF4jContext ctx) {
        super(ctx.getFf4j(), ctx.getParameters());
        this.feature = feature;
    }
    
    /**
     * Getter accessor for attribute 'feature'.
     *
     * @return
     *       current value of 'feature'
     */
    public Feature getFeature() {
        return feature;
    }

    /**
     * Setter accessor for attribute 'feature'.
     * @param feature
     * 		new value for 'feature '
     */
    public void setFeature(Feature feature) {
        this.feature = feature;
    }

}
