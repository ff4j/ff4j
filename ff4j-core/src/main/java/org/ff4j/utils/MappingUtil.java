package org.ff4j.utils;

import java.util.HashMap;

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

import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.property.PropertyString;
import org.ff4j.property.PropertyBigDecimal;
import org.ff4j.property.PropertyBigInteger;
import org.ff4j.property.PropertyBoolean;
import org.ff4j.property.PropertyByte;
import org.ff4j.property.PropertyCalendar;
import org.ff4j.property.PropertyDate;
import org.ff4j.property.PropertyDouble;
import org.ff4j.property.PropertyFloat;
import org.ff4j.property.PropertyInt;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLong;
import org.ff4j.property.PropertyShort;

/**
 * Utility class to work with parameters.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class MappingUtil {

    /** Separator to propose parameters. */
    private static final String SEPARATOR = "&";
    
    /** Primitive mapping. */
    private static Map < String, String > PROPERTY_TYPES;

    private static ClassLoader classLoader;

    /**
     * Hiding default constructor for utility class.
     */
    private MappingUtil() {}
    
    /** Substitution in XML or any store. */
    
    /**
     * Initialisation of substitution types
     */
    static {
        PROPERTY_TYPES = new HashMap<String, String >();
        PROPERTY_TYPES.put("byte",       PropertyByte.class.getName());
        PROPERTY_TYPES.put("boolean",    PropertyBoolean.class.getName());
        PROPERTY_TYPES.put("bigdecimal", PropertyBigDecimal.class.getName());
        PROPERTY_TYPES.put("biginteger", PropertyBigInteger.class.getName());
        PROPERTY_TYPES.put("calendar",   PropertyCalendar.class.getName());
        PROPERTY_TYPES.put("date",       PropertyDate.class.getName());
        PROPERTY_TYPES.put("double",     PropertyDouble.class.getName());
        PROPERTY_TYPES.put("float",      PropertyFloat.class.getName());
        PROPERTY_TYPES.put("int",        PropertyInt.class.getName());
        PROPERTY_TYPES.put("loglevel",   PropertyLogLevel.class.getName());
        PROPERTY_TYPES.put("short",      PropertyShort.class.getName());
        PROPERTY_TYPES.put("long",       PropertyLong.class.getName());
        PROPERTY_TYPES.put("string",     PropertyString.class.getName());
    }
    
    /**
     * Substitution of primitive into PropertyXXX.
     * Allows to TODO
     * @param pType
     * @return
     */
    public static String mapPropertyType(String pType) {
        if (pType == null) return null;
        if (PROPERTY_TYPES.containsKey(pType.toLowerCase())) {
           return PROPERTY_TYPES.get(pType.toLowerCase());
        }
        return pType;
    }
    
    /**
     * Substitution of PropertyXXX intoPrimitive.
     *
     * Allows to TODO
     * @param pType
     * @return
     */
    public static String mapSimpleType(String className) {
        if (className == null) return className;
        if (PROPERTY_TYPES.containsValue(className)) {
           return Util.getFirstKeyByValue(PROPERTY_TYPES, className);
        }
        return className;
    }

    /**
     * Substitution of primitive into PropertyXXX.
     * Allows to TODO
     * @param pType
     * @return
     */
    public static String mapSimpleType(Class<?> pType) {
        if (pType == null) return null;
        return mapSimpleType(pType.getName());
    }

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
    
    /**
     * Instanciate flipping strategy from its class name.
     *
     * @param className
     *      current class name
     * @return
     *      the flipping strategy
     */
    @SuppressWarnings("unchecked")
	public static FlippingStrategy instanceFlippingStrategy(String uid, String className,  Map<String, String> initparams) {
        try {
            Class<FlippingStrategy> clazz = (Class<FlippingStrategy>) (classLoader == null ? Class.forName(className) : classLoader.loadClass(className));
            FlippingStrategy flipStrategy = clazz.newInstance();
            flipStrategy.init(uid, initparams);
            return flipStrategy;
        } catch (Exception ie) {
            throw new FeatureAccessException("Cannot instantiate Strategy, no default constructor available", ie);
        } 
    }

    public static void setClassLoader(ClassLoader classLoader) {
        MappingUtil.classLoader = classLoader;
    }
}
