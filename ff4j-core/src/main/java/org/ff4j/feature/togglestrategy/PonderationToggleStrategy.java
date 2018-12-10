package org.ff4j.feature.togglestrategy;

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
