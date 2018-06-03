package org.ff4j.feature.strategy;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import static org.ff4j.utils.JsonUtils.mapAsJson;
import static org.ff4j.utils.JsonUtils.valueAsJson;

import java.util.Map;

import org.ff4j.property.exception.PropertyAccessException;

/**
 * Pattern strategy, implementing evaluation at runtime (status or value).
 *
 * @author Cedrick LUNVEN  (@clunven)
 * 
 * @since 2.x
 */
public interface FF4jToggleStrategy {
    
    /**
     * Allow to parameterized Flipping Strategy
     * 
     * @param featureName
     *            current featureName
     * @param initValue
     *            initial Value
     */
    void init(String uid, Map<String, String> initParam);

    /**
     * Initial Parameters required to insert this new flipping.
     * 
     * @return initial parameters for this strategy
     */
    Map<String, String> getInitParams();
    
    /**
     * Instanciate flipping strategy from its class name.
     *
     * @param className
     *      current class name
     * @return
     *      the flipping strategy
     */
    static FF4jToggleStrategy of(String uid, String className,  Map<String, String> initparams) {
        try {
            FF4jToggleStrategy evalStrategy = (FF4jToggleStrategy) Class.forName(className).newInstance();
            evalStrategy.init(uid, initparams);
            return evalStrategy;
        } catch (Exception ie) {
            throw new PropertyAccessException("Cannot instantiate Strategy, no default constructor available", ie);
        } 
    }
    /**
     * Generate flipping strategy as json.
     * 
     * @return
     *      flippling strategy as json.     
     */
     default String toJson() {
        StringBuilder json = new StringBuilder("{");
        json.append(valueAsJson("initParams") + ":");
        json.append(mapAsJson(getInitParams()));
        json.append("," + valueAsJson("type")  + ":");
        json.append(valueAsJson(getClass().getCanonicalName()));
        json.append("}");
        return json.toString();
    }

}
