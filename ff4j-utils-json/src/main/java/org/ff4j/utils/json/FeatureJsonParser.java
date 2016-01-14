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
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.property.AbstractProperty;
import org.ff4j.property.Property;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
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
        f.setCustomProperties(parseCustomProperties(f.getUid(), propertyMap));
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
    private static Map < String, AbstractProperty<?>> parseCustomProperties(String uid, Map <String, Object > customPTag) {
        Map < String, AbstractProperty<?>> myProperties = new LinkedHashMap<String, AbstractProperty<?>>();
        if (null != customPTag && !customPTag.isEmpty()) {
            // Loop over properties
            for (Object property : customPTag.values()) {
                HashMap<String, Object> propertyJson = (HashMap<String, Object>) property;
                String propertyName = (String) propertyJson.get("name");
                String propertyVal  = String.valueOf(propertyJson.get("value"));
                AbstractProperty<?> ap = new Property(propertyName, propertyVal);
                
                // Dedicated Type
                String propertyType = (String) propertyJson.get("type");
                if (propertyType != null) {
                    try {
                        // Construction by dedicated constructor with introspection
                        Constructor<?> constr = Class.forName(propertyType).getConstructor(String.class, String.class);
                        ap = (AbstractProperty<?>) constr.newInstance(propertyName, propertyVal);
                    } catch (InstantiationException e) {
                        throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "' check default constructor", e);
                    } catch (IllegalAccessException e) {
                        throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "' check visibility", e);
                    } catch (ClassNotFoundException e) {
                        throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "' not found", e);
                    } catch (InvocationTargetException e) {
                        throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "'  error within constructor", e);
                    } catch (NoSuchMethodException e) {
                        throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "' constructor not found", e);
                    } catch (SecurityException e) {
                        throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "' check constructor visibility", e);
                    }
                }
                
                //  Is there any fixed Value ?
                List <Object> listOfFixedValue = (List<Object>) propertyJson.get("fixedValues");
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
                myProperties.put(ap.getName(),ap);
            }
        }
        return myProperties;
        
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
        if (null == json || "".equals(json)) return null;
        try {
            return parseFlipStrategy(uid, (HashMap<String, Object>) objectMapper.readValue(json, HashMap.class));
        } catch (JsonParseException e) {
            throw new IllegalArgumentException("Cannot parse JSON " + json, e);
        } catch (JsonMappingException e) {
            throw new IllegalArgumentException("Cannot map JSON " + json, e);
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot read JSON " + json, e);
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
    public static FlippingStrategy parseFlipStrategy(String uid, HashMap<String, Object> flipMap) {
        if (null == flipMap || flipMap.isEmpty()) return null;
        String classType = null;
        FlippingStrategy strategy = null;
        try {
            //Map<String, Object> flipMap = objectMapper.readValue(json, HashMap.class);
            classType = (String) flipMap.get("type");
            strategy = (FlippingStrategy) Class.forName(classType).newInstance();
            HashMap<String, String> initparams = (HashMap<String, String>) flipMap.get("initParams");
            // Initialized
            strategy.init(uid, initparams);
        } catch (InstantiationException e) {
            throw new IllegalArgumentException(classType + " does not seems to have a DEFAULT constructor", e);
        } catch (IllegalAccessException e) {
            throw new IllegalArgumentException(classType + " does not seems to have a PUBLIC constructor", e);
        } catch (ClassNotFoundException e) {
            throw new IllegalArgumentException(classType + " has not been found within classpath, check syntax", e);
        }
        return strategy;
    }
    
    /**
     * Initialization of FlipStrategy from elements.
     * 
     * @param featureUid
     *      current feature id
     * @param classType
     *      current type of class
     * @param initparams
     *      current init parameters
     * @return
     *      a flipping strategy
     */
    public static FlippingStrategy parseFlipStrategy(String featureUid, String classType, HashMap<String, String> initparams) {
        FlippingStrategy strategy = null;
        try {            
            strategy = (FlippingStrategy) Class.forName(classType).newInstance();
            strategy.init(featureUid, initparams);
        } catch (InstantiationException e) {
            throw new IllegalArgumentException(classType + " does not seems to have a DEFAULT constructor", e);
        } catch (IllegalAccessException e) {
            throw new IllegalArgumentException(classType + " does not seems to have a PUBLIC constructor", e);
        } catch (ClassNotFoundException e) {
            throw new IllegalArgumentException(classType + " has not been found within classpath, check syntax", e);
        }
        return strategy;
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
        if (null == json || "".equals(json))
            return null;
        try {
            List<LinkedHashMap<String, Object>> flipMap = objectMapper.readValue(json, List.class);
            Feature[] fArray = new Feature[flipMap.size()];
            int idx = 0;
            for (LinkedHashMap<String, Object> ll : flipMap) {
                fArray[idx++] = parseFeatureMap(ll);
            }
            return fArray;
        } catch (JsonParseException e) {
            throw new IllegalArgumentException("Cannot parse JSON " + json, e);
        } catch (JsonMappingException e) {
            throw new IllegalArgumentException("Cannot map JSON " + json, e);
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot read JSON " + json, e);
        }
    }

}
