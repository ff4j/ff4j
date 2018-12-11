package org.ff4j.utils.json;

/*-
 * #%L
 * ff4j-utils-json
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

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.feature.Feature;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyFactory;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Unmarshalling data from JSON with Jackson.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class PropertyJsonParser {

    public static String UID                          = "uid";
    public static final String PROPERTY_VALUE         = "value";
    public static final String PROPERTY_READONLY      = "readOnly";
    public static final String PROPERTY_TYPE          = "type";
    public static final String PROPERTY_FIXED_VALUES  = "fixedValues";
    public static final String DESCRIPTION            = "description";
    
    /** Jackson mapper. */
    private static ObjectMapper objectMapper = new ObjectMapper();
    
    private PropertyJsonParser() {}
    
    /**
     * Parse the "customproperties" JSOn attribute.
     *
     * @param uid
     *      current feature identifier
     * @param tag
     *      current TAG
     * @return
     *      target map of properties
     */
    @SuppressWarnings("unchecked")
    public static Map < String, Property<?>> parseProperties(Map <String, Object > customPTag) {
        Map < String, Property<?>> myProperties = new LinkedHashMap<String, Property<?>>();
        if (null != customPTag && !customPTag.isEmpty()) {
            // Loop over properties
            for (Object property : customPTag.values()) {
                Property<?> ap = PropertyJsonParser.parsePropertyTag((HashMap<String, Object>) property);
                myProperties.put(ap.getUid(), ap);
            }
        }
        return myProperties;
    }
    
    
    /**
     * Parse the json expression as array of {@link Feature}.
     *
     * @param json
     *      json expression
     * @return
     *      array of feature
     */
    @SuppressWarnings("unchecked")
    public static Property<?>[] parsePropertyArray(String json) {
        if (null == json || "".equals(json)) {
            return null;
        }
        try {
            List<LinkedHashMap<String, Object>> flipMap = objectMapper.readValue(json, List.class);
            Property<?>[] fArray = new Property<?>[flipMap.size()];
            int idx = 0;
            for (LinkedHashMap<String, Object> ll : flipMap) {
                fArray[idx++] = parsePropertyTag(ll);
            }
            return fArray;
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse JSON " + json, e);
        }
    }
    
    /**
     * Unmarshall {@link Feature} from json string.
     *
     * @param json
     *            json representation of feature.
     * @return feature object
     */
    @SuppressWarnings("unchecked")
    public static Property<?> parseJsonProperty(String json) {
        try {
            return parsePropertyTag(objectMapper.readValue(json, HashMap.class));
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot parse json as Property " + json, e);
        }
    }
    
    @SuppressWarnings("unchecked")
    public static Property<?> parsePropertyTag(HashMap<String, Object> property) {
        HashMap<String, Object> propertyJson = (HashMap<String, Object>) property;
        String propertyName = (String) propertyJson.get(UID);
        String propertyVal  = String.valueOf(propertyJson.get(PROPERTY_VALUE));
        String propertyType = (String) propertyJson.get(PROPERTY_TYPE);
        Property<?> ap = PropertyFactory.createProperty(propertyName, propertyType, propertyVal);
        // Description
        ap.setDescription((String)propertyJson.get(DESCRIPTION));
        // FixedValued
        addFixedValuesToProperty(ap, (List<Object>) propertyJson.get(PROPERTY_FIXED_VALUES));
        return ap;
    }

    public static void addFixedValuesToProperty(Property<?> ap, List<Object> listOfFixedValue) {
        if (listOfFixedValue != null) {
            for (Object v : listOfFixedValue) {
                ap.add2FixedValueFromString(String.valueOf(v));
            }
            // Check fixed value
            if (ap.getFixedValues().isPresent() && !ap.getFixedValues().get().contains(ap.getValue())) {
                throw new IllegalArgumentException("Cannot create property <" + ap.getUid() + 
                        "> invalid value <" + ap.getValue() + 
                        "> expected one of " + ap.getFixedValues());
            }
        }
    }

}
