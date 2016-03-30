package org.ff4j.utils.json;

import java.util.ArrayList;

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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.property.util.PropertyJsonBean;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Unmarshalling data from JSON with Jackson.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class PropertyJsonParser {

    public static final String FIXED_VALUES = "fixedValues";
    /** Jackson mapper. */
    private static ObjectMapper objectMapper = new ObjectMapper();
    
    private PropertyJsonParser() {}
    
    /**
     * Unmarshall {@link Feature} from json string.
     *
     * @param json
     *            json representation of feature.
     * @return feature object
     */
    @SuppressWarnings("unchecked")
    public static Property<?> parseProperty(String json) {
        if (null == json || "".equals(json)) {
            return null;
        }
        Map<String, Object> propertyJson;
        try {
            propertyJson = objectMapper.readValue(json, HashMap.class);
        } catch (Exception re) {
            throw new IllegalArgumentException("Cannot parse JSON Property", re);
        }
        String propertyName = String.valueOf(propertyJson.get("name"));
        String propertyVal  = String.valueOf(propertyJson.get("value"));
        String propertyType = String.valueOf(propertyJson.get("type"));
        Property < ?> ap = PropertyFactory.createProperty(propertyName, propertyType, propertyVal);
        
        // FixedValued
        List <Object> listOfFixedValue = (List<Object>) propertyJson.get(FIXED_VALUES);
        if (listOfFixedValue != null) {
            for (Object v : listOfFixedValue) {
                ap.add2FixedValueFromString(String.valueOf(v));
            }
            // Check fixed value
            if (ap.getFixedValues() != null && !ap.getFixedValues().contains(ap.getValue())) {
                throw new IllegalArgumentException("Cannot create property <" + ap.getName() + 
                        "> invalid value <" + ap.getValue() + 
                        "> expected one of " + ap.getFixedValues());
            }
        }
       return ap;
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
                fArray[idx++] = parsePropertyMap(ll);
            }
            return fArray;
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse JSON " + json, e);
        }
    }
    
    /**
     * Map of property.
     *
     * @param fMap
     *      map of properties
     * @return
     *      property
     */
    @SuppressWarnings("unchecked")
    public static Property<?> parsePropertyMap(Map<String, Object> fMap) {
        PropertyJsonBean pf = new PropertyJsonBean();
        pf.setName((String) fMap.get("name"));
        pf.setDescription((String) fMap.get("description"));
        pf.setType((String) fMap.get("type"));
        pf.setValue((String) fMap.get("value"));
        if (fMap.containsKey(FIXED_VALUES)) {
            List < String > dbList = (ArrayList<String>) fMap.get(FIXED_VALUES);
            if (dbList != null) {
                for(Object item : dbList) {
                    pf.addFixedValue((String) item);
                }
            }
        }
        return pf.asProperty();
    }    

}
