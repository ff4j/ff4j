package org.ff4j.feature.togglestrategy.time;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.ff4j.feature.togglestrategy.AbstractToggleStrategy;
import org.ff4j.feature.togglestrategy.ToggleContext;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyDate;
import org.ff4j.property.PropertyString;

/**
 * The feature will be flipped after release date is reached.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ReleaseDateToggleStrategy extends AbstractToggleStrategy {
    
    /** Serial. */
    private static final long serialVersionUID = -6269110517013603914L;

    /** Constant for release Date. */
    private static final String PARAMNAME_RELEASEDATE = "releaseDate";

    /** Release Date. */
    private Date releaseDate = new Date();

    /** {@inheritDoc} */
    @Override
    public void initialize() {
        Property<?> p = getRequiredProperty(PARAMNAME_RELEASEDATE);
        
        // Parsing V1 file with Map<String, String> and not typed params
        if (p instanceof PropertyString) {
            try {
                releaseDate = new SimpleDateFormat("yyyy-MM-dd-HH:mm").parse(p.asString());
            } catch (ParseException e) {
                throw new IllegalArgumentException("Cannot parse release "
                        + "date expected format is 'yyyy-MM-dd-HH:mm'",e);
            }
        } else {
            releaseDate = ((PropertyDate) p).getValue();
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean test(ToggleContext ctx) {
        return new Date().after(releaseDate);
    }
    
}
