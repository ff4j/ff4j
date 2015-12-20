package org.ff4j.utils.json;

/*
 * #%L
 * ff4j-utils-json
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.property.AbstractProperty;
import org.ff4j.property.Property;

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
    public static AbstractProperty<?> parseProperty(String json) {
        Map<String, Object> propertyJson;
        try {
            propertyJson = objectMapper.readValue(json, HashMap.class);
        } catch (Exception re) {
            throw new IllegalArgumentException("Cannot parse JSON Property", re);
        }
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
        return ap;
    }

}
