package org.ff4j.strategy;

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

import org.ff4j.core.FlippingStrategy;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.MappingUtil;

/**
 * Super class for {@link FlippingStrategy} implementation with utilities.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public abstract class AbstractFlipStrategy implements FlippingStrategy {

    /** Initial parameters. */
    protected Map<String, String> initParams = new HashMap<String, String>();

    /** ClassType. */
    private final String type = getClass().getName();
    
    /** {@inheritDoc} */
    @Override
    public void init(String featureName, Map<String, String> initParam) {
        this.initParams = initParam;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, String> getInitParams() {
        return this.initParams;
    }

    /**
     * Check presence of expected parameter.
     * 
     * @param paramName
     *            target parameter name
     */
    public void assertRequiredParameter(String paramName) {
        if (!initParams.containsKey(paramName)) {
            String msg = String.format("Parameter '%s' is required for this FlippingStrategy", paramName);
            throw new IllegalArgumentException(msg);
        }
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return this.getType() + "," + MappingUtil.fromMap(getInitParams());
    }
    
    /**
     * Convert {@link FlippingStrategy} as Json.
     *
     * @return
     *      the relative string
     */
    public String toJson() {
        return JsonUtils.flippingStrategyAsJson(this);
    }
    

    /**
     * Getter accessor for attribute 'classType'.
     * 
     * @return current value of 'classType'
     */
    public String getType() {
        return type;
    }

}
