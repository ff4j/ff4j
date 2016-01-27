package org.ff4j.utils.json;

/*
 * #%L
 * ff4j-utils-json
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Unmarshalling data from JSON with Jackson.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyJsonParser {

    /** Jackson mapper. */
    private static ObjectMapper objectMapper = new ObjectMapper();

    /**
     * Unmarshall {@link Feature} from json string.
     *
     * @param json
     *            json representation of feature.
     * @return feature object
     */
    @SuppressWarnings("unchecked")
    public static Property<?> parseProperty(String json) {
        if (null == json || "".equals(json)) return null;
        Map<String, Object> propertyJson;
        try {
            propertyJson = objectMapper.readValue(json, HashMap.class);
        } catch (Exception re) {
            throw new IllegalArgumentException("Cannot parse JSON Property", re);
        }
        String propertyName = String.valueOf(propertyJson.get("name"));
        String propertyVal  = String.valueOf(propertyJson.get("value"));
        String propertyType = String.valueOf(propertyJson.get("type"));
        return PropertyFactory.createProperty(propertyName, propertyType, propertyVal);
   }

}
