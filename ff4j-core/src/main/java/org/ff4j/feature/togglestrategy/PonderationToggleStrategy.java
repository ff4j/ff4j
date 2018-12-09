package org.ff4j.feature.togglestrategy;

import org.ff4j.property.Property;
import org.ff4j.property.PropertyDouble;
import org.ff4j.property.PropertyString;

/**
 * Toggle for a random subset 
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class PonderationToggleStrategy extends AbstractToggleStrategy {

    /** Serial number. */
    private static final long serialVersionUID = -2353911851539414159L;

    /** Params. */
    public static final String PARAM_WEIGHT = "weight";
    
    /** Expected Parameters. */
    private Double weight = null;
    
    /** {@inheritDoc} */
    @Override
    public void initialize() {
        Property<?> p = getRequiredProperty(PARAM_WEIGHT);
        // Parsing V1 file with Map<String, String> and not typed params
        if (p instanceof PropertyString) {
            weight = p.asDouble();
        } else {
            weight = ((PropertyDouble) p).getValue();
        }
        if (weight < 0 || weight > 1) {
            throw new IllegalArgumentException("Weight is a percentage and should be between 0 and 1");
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean test(ToggleContext ctx) {
        return Math.random() <= weight;
    }
    
}
