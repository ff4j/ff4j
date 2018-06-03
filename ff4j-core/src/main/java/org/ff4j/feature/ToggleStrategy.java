package org.ff4j.feature;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.ff4j.FF4jContext;
import org.ff4j.feature.exception.InvalidStrategyTypeException;
import org.ff4j.feature.strategy.FF4jToggleStrategy;

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
public interface ToggleStrategy extends FF4jToggleStrategy {
    
    /** Separator to propose parameters. */
    String SEPARATOR = "&";
   
    /**
     * Tell if flip should be realized.
     * 
     * @param featureName
     *            target featureName
     * @param executionContext
     *            custom params to make decision
     * @return if flipping should be performed
     */
    boolean isToggled(Feature feature, FF4jContext executionContext);
    
    /**
     * Instanciate flipping strategy from its class name.
     *
     * @param className
     *      current class name
     * @return
     *      the flipping strategy
     */
    public static ToggleStrategy of(String uid, String className,  Map<String, String> initparams) {
        try {
            ToggleStrategy strategy = (ToggleStrategy) Class.forName(className).newInstance();
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
     */
    public static String fromMap(Map < String, String > params) {
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
    }

    /**
     * Utility method to convert parameters as Map
     * 
     * @param strParam
     *            convert String param as Map.
     * @return map of parameters.
     */
    public static Map<String, String> toMap(String strParam) {
        LinkedHashMap<String, String> parameters = new LinkedHashMap<String, String>();
        if (strParam != null) {
            String[] chunks = strParam.split("\\" + SEPARATOR);
            for (String chunk : chunks) {
                int idxEqual = chunk.indexOf("=");
                if (idxEqual > 0 && idxEqual < chunk.length()) {
                    String paramName = chunk.substring(0, idxEqual);
                    String paramValue = chunk.substring(idxEqual + 1);
                    parameters.put(paramName, paramValue);
                }
            }
        }
        return parameters;
    }
    
}
