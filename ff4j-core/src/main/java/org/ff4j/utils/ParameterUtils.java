package org.ff4j.utils;

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

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Utility class to work with parameters.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ParameterUtils {

    /** Separator to propose parameters. */
    private static final String SEPARATOR = "&";

    /**
     * Hiding default constructor for utility class.
     */
    private ParameterUtils() {}

    /**
     * Utility Method to convert Parameter Map into String.
     * 
     * @param params
     *            parameter MAP
     * @return parameters as String
     */
    public static final String fromMap(Map < String, String > params) {
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
    public static final Map<String, String> toMap(String strParam) {
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
