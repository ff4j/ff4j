package org.ff4j.core;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import java.io.Serializable;
import java.util.Map;

/**
 * Each feature should implement flipping strategy. (enabling/disabling will be handled by flipper.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public interface FlippingStrategy extends Serializable {

    /**
     * Allows adding parameters to Flipping Strategy
     * 
     * @param featureName
     *            current featureName
     * @param initParam
     *            initial Value
     */
    void init(String featureName, Map<String, String> initParam);

    /**
     * Initial Parameters required to insert this new flipping.
     * 
     * @return initial parameters for this strategy
     */
    Map<String, String> getInitParams();

    /**
     * Tell if flip should be realized.
     * 
     * @param featureName
     *            target featureName
     * @param executionContext
     *            custom params to make decision
     * @return if flipping should be performed
     */
    boolean evaluate(String featureName, FeatureStore store, FlippingExecutionContext executionContext);

}
