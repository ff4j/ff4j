package org.ff4j.feature.togglestrategy.time;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2018 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

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
