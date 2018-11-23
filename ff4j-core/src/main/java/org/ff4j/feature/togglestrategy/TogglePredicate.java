package org.ff4j.feature.togglestrategy;

import static org.ff4j.utils.JsonUtils.mapAsJson;
import static org.ff4j.utils.JsonUtils.valueAsJson;

import java.util.Map;
import java.util.function.Predicate;

import org.ff4j.feature.exception.InvalidStrategyTypeException;

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

/**
 * Each feature should implement the flipping strategy. (enabling/disabling will be handle by flipper.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public interface TogglePredicate extends Predicate< ToggleContext > {
            
    /**
     * Predicate with 2 params
     * 
     * @param ctx
     *          toggle context
     * @return
     *      if the feature would be toggled or not
     */
    boolean test(ToggleContext ctx);
    
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
     * Generate flipping strategy as json.
     * 
     * @return
     *      flippling strategy as json.     
     */
     default String toJson() {
        StringBuilder json = new StringBuilder("{");
        json.append(valueAsJson("params") + ":");
        json.append(mapAsJson(getInitParams()));
        json.append("," + valueAsJson("className")  + ":");
        json.append(valueAsJson(getClass().getCanonicalName()));
        json.append("}");
        return json.toString();
    }
     
    /**
     * Instanciate flipping strategy from its class name.
     *
     * @param className
     *      current class name
     * @return
     *      the flipping strategy
     */
    public static TogglePredicate of(String uid, String className,  Map<String, String> initparams) {
        try {
            TogglePredicate strategy = (TogglePredicate) Class.forName(className).newInstance();
            strategy.init(uid, initparams);
            return strategy;
        } catch (Exception ie) {
            throw new InvalidStrategyTypeException(className, ie);
        }
    }
    
    /**
     * Utility Method to convert Parameter Map into String.
     * 
     * @param params
     *            parameter MAP
     * @return parameters as String
     *
    public static ToggleStrategy fromMap(Map < String, String > params) {
        return JsonUtils.mapAsJson(params);
        StringBuilder strBulBuilder = new StringBuilder();
        boolean first = true;
        if (params != null && !params.isEmpty()) {
            for (Entry<String, String> entry : params.entrySet()) {
                if (!first) {
                    strBulBuilder.append(SEPARATOR);
                }
                strBulBuilder.append(entry.getKey() + "=" + entry.getValue());
                first = false;
            }
        }
        return strBulBuilder.toString();
    }*/

   
    
}
