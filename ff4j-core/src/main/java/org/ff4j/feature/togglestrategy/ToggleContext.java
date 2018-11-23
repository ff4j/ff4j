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
