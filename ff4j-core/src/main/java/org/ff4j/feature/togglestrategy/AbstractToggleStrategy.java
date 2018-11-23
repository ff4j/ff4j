package org.ff4j.feature.togglestrategy;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
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

import java.util.HashMap;
import java.util.Map;

/**
 * Super class for {@link TogglePredicate} implementation with utilities.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public abstract class AbstractToggleStrategy implements TogglePredicate {

    /** Parameters. */
    protected Map<String, String> params = new HashMap<String, String>();
    
    /** {@inheritDoc} */
    @Override
    public void init(String featureName, Map<String, String> initParam) {
        this.params = initParam;
    }
    
    /**
     * Check presence of expected parameter.
     * 
     * @param paramName
     *            target parameter name
     */
    public void assertParam(String paramName) {
        if (!params.containsKey(paramName)) {
            String msg = String.format("Parameter '%s' is required for this FlippingStrategy", paramName);
            throw new IllegalArgumentException(msg);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, String> getInitParams() {
        return this.params;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }
}
