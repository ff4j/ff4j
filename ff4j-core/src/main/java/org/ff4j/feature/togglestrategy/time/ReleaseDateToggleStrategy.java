package org.ff4j.feature.togglestrategy.time;

import org.ff4j.backend.Backend;
import org.ff4j.feature.Flag;
import org.ff4j.feature.togglestrategy.AbstractToggleStrategy;
import org.ff4j.property.PropertyDate;
import org.ff4j.property.evaluate.FF4jEvaluationContext;

import java.util.Date;

/**
 * The feature will be flipped after release date is reached.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ReleaseDateToggleStrategy extends AbstractToggleStrategy {

    /** Constant for release Date. */
    private static final String RELEASE_DATE = "releaseDate";

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
    public ReleaseDateToggleStrategy(Backend backend, Flag relatedFeature, FF4jEvaluationContext config) {
        this(backend, relatedFeature, (Date) config.getProperty(RELEASE_DATE).getValue());
    }

    /**
     * Constructor with everything needed
     *
     * @param backend
     *      current backend
     * @param relatedFeature
     *      current feature
     * @param release
     *      release
     */
    public ReleaseDateToggleStrategy(Backend backend, Flag relatedFeature, Date release) {
        super(backend, relatedFeature, new FF4jEvaluationContext(new PropertyDate(RELEASE_DATE, release)));
    }

    /** {@inheritDoc} */
    @Override
    public boolean test(FF4jEvaluationContext ctx) {
        return new Date().after((Date) getConfig().getProperty(RELEASE_DATE).getValue());
    }
    
}
