package org.ff4j.utils.json;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.MappingUtil;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Unmarshalling data from JSON with Jackson.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureJsonParser {

    /** Jackson mapper. */
    private static ObjectMapper objectMapper = new ObjectMapper();
    
    /**
     * Hide constructor.
     */
    private FeatureJsonParser() {
    }
    
    /**
     * Unmarshall {@link Feature} from json string.
     *
     * @param json
     *            json representation of feature.
     * @return feature object
     */
    @SuppressWarnings("unchecked")
    public static Feature parseFeature(String json) {
        try {
            return parseFeatureMap(objectMapper.readValue(json, HashMap.class));
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot parse json as Feature " + json, e);
        }
    }
    
    @SuppressWarnings("unchecked")
    public static Set<String> parsePermissions(String json) {
        if (json == null) return null;
        try {
            return objectMapper.readValue(json, Set.class);
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse json list");
        }
    }

    @SuppressWarnings("unchecked")
    private static Feature parseFeatureMap(Map<String, Object> fMap) {
        Feature f = new Feature((String) fMap.get("uid"));
        f.setEnable((Boolean) fMap.get("enable"));
        f.setDescription((String) fMap.get("description"));
        f.setGroup((String) fMap.get("group"));
        // permissions
        List<String> perm = (ArrayList<String>) fMap.get("permissions");
        f.setPermissions(new HashSet<String>());
        if (perm != null) {
            f.getPermissions().addAll(perm);
        }
        // flipping strategy
        f.setFlippingStrategy(parseFlipStrategy(f.getUid(), (LinkedHashMap<String, Object>) fMap.get("flippingStrategy")));
        // custom properties
        Map <String, Object > propertyMap = (Map < String, Object >) fMap.get("customProperties");
        f.setCustomProperties(parseCustomProperties(propertyMap));
        return f;
    }    
    
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
    private static Map < String, Property<?>> parseCustomProperties(Map <String, Object > customPTag) {
        Map < String, Property<?>> myProperties = new LinkedHashMap<String, Property<?>>();
        if (null != customPTag && !customPTag.isEmpty()) {
            // Loop over properties
            for (Object property : customPTag.values()) {
                HashMap<String, Object> propertyJson = (HashMap<String, Object>) property;
                String propertyName = (String) propertyJson.get("name");
                String propertyVal  = String.valueOf(propertyJson.get("value"));
                String propertyType = (String) propertyJson.get("type");
                Property<?> ap = PropertyFactory.createProperty(propertyName, propertyType, propertyVal);
                // FixedValued
                List <Object> listOfFixedValue = (List<Object>) propertyJson.get("fixedValues");
                addFixedValuesToProperty(ap, listOfFixedValue);
                myProperties.put(ap.getName(), ap);
            }
        }
        return myProperties;
        
    }

    private static void addFixedValuesToProperty(Property<?> ap, List<Object> listOfFixedValue) {
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
    }

    /**
     * Convert feature array to json.
     *
     * @param features
     *            target features
     * @return json string
     */
    public static String featureArrayToJson(Feature[] features) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        if (features != null) {
            boolean first = true;
            for (Feature feature : features) {
                sb.append(first ? "" : ",");
                sb.append(feature.toJson());
                first = false;
            }
        }
        sb.append("]");
        return sb.toString();
    }

    /**
     * Parse json string to get {@link FlippingStrategy}.
     * 
     * @param uid
     *            identifier
     * @param json
     *            json expression
     * @return flip strategy
     */
    @SuppressWarnings("unchecked")
    public static FlippingStrategy parseFlipStrategyAsJson(String uid, String json) {
        if (null == json || "".equals(json)) {
            return null;
        }
        try {
            return parseFlipStrategy(uid, (HashMap<String, Object>) objectMapper.readValue(json, HashMap.class));
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse JSON " + json, e);
        }
    }
    
    /**
     * Parse json string to get {@link FlippingStrategy}.
     * 
     * @param uid
     *            identifier
     * @param json
     *            json expression
     * @return flip strategy
     */
    @SuppressWarnings("unchecked")
    public static FlippingStrategy parseFlipStrategy(String uid, Map<String, Object> flipMap) {
        if (null == flipMap || flipMap.isEmpty()) {
            return null;
        }
        String classType = (String) flipMap.get("type");
        HashMap<String, String> initparams = (HashMap<String, String>) flipMap.get("initParams");
        return MappingUtil.instanceFlippingStrategy(uid, classType, initparams);
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
    public static Feature[] parseFeatureArray(String json) {
        if (null == json || "".equals(json)) {
            return null;
        }
        try {
            List<LinkedHashMap<String, Object>> flipMap = objectMapper.readValue(json, List.class);
            Feature[] fArray = new Feature[flipMap.size()];
            int idx = 0;
            for (LinkedHashMap<String, Object> ll : flipMap) {
                fArray[idx++] = parseFeatureMap(ll);
            }
            return fArray;
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse JSON " + json, e);
        }
    }

}
