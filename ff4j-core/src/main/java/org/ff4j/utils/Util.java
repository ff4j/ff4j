package org.ff4j.utils;

import java.lang.reflect.Constructor;

/*
 * #%L
 * ff4j-core
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 * Tips and tricks to be less verbose.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class Util {
    
    /** Start Color. */
    private static final String START_COLOR = "00AB8B";
    
    /** End Color. */
    private static final String END_COLOR = "EEFFEE";

    private Util() {
    }
    
    /**
     * Check that expression is true.
     * 
     * @param expression
     *            expression to evaluate
     */
    public static boolean hasLength(String expression) {
        return expression != null && !"".equals(expression);
    }
    
   /**
     * Check that expression is true.
     * 
     * @param expression
     *            expression to evaluate
     */
    public static void assertTrue(boolean expression) {
        if (!expression) {
            throw new IllegalArgumentException("[Assertion failed] - this expression must be true");
        }
    }

    /**
     * Check that object is null.
     * 
     * @param object
     *            target object
     */
    public static void assertNull(Object object) {
        if (object != null) {
            throw new IllegalArgumentException("[Assertion failed] - the object argument must be null");
        }
    }

    /**
     * Check that object is not null.
     * 
     * @param object
     *            target object
     */
    public static void assertNotNull(Object... params) {
        if (params != null) {
            for (int idx = 0; idx < params.length ;idx++) {
                Object currentparam = params[idx];
                if (null == currentparam) {
                    throw new IllegalArgumentException("[Assertion failed] - Parameter #" + idx + "(object)  must not be null");
                }
            }
        }
    }

    /**
     * Check that string is not null
     * 
     * @param object
     *            target object
     */
    public static void assertHasLength(String... params) {
        if (params != null) {
            for (int idx = 0; idx < params.length ;idx++) {
                String currentparam = params[idx];
                if (null == currentparam || currentparam.isEmpty()) {
                    throw new IllegalArgumentException("[Assertion failed] - Parameter #" + idx + "(string)  must not be null nor empty");
                }
            }
        }
    }
    
    /**
     * Check that string is not null
     * 
     * @param object
     *            target object
     */
    public static void assertNotEmpty(Collection<?> collec) {
        if (null == collec || collec.isEmpty()) {
            throw new IllegalArgumentException("[Assertion failed] - Target COLLECTION must not be null nor empty");
        }
    }
    
    /**
     * Parameter validation.
     *
     * @param param
     *      current parameter
     * @param paramName
     *      current parameter name
     */
    public static void assertParamNotNull(String param, String paramName) {
        if (param == null || param.isEmpty()) {
            throw new IllegalArgumentException("Missing Parameter '" + paramName + "' must not be null nor empty");
        }
    }

    /**
     * Create an HashSet.
     *
     * @param els
     *            enumeration of elements
     * @return
     */
	public static <T> Set<T> set(T... els) {
        return new HashSet<T>(Arrays.asList(els));
    }
	
	/**
     * Create an HashSet.
     *
     * @param els
     *            enumeration of elements
     * @return
     */
    public static <T> List<T> list(T... els) {
        return new ArrayList<T>(Arrays.asList(els));
    }
    
    /**
     * This code build the color gradient between 2 colors with defined step.
     * @param codeFrom
     *      color source
     * @param codeTo
     *      color destination
     * @param nbDivision
     *      number of steps
     * @return
     *      the list of colors
     */
    private static List < String > getColorGradient(String codeFrom, String codeTo, int nbDivision) {
        List < String > colors = new ArrayList<String>();
        if (nbDivision > 0) {
            int rStart = Integer.parseInt(codeFrom.substring(0, 2), 16);
            int rDelta = (Integer.parseInt(codeTo.substring(0, 2), 16) - rStart) / nbDivision;
            int gStart = Integer.parseInt(codeFrom.substring(2, 4), 16);
            int gDelta = (Integer.parseInt(codeTo.substring(2, 4), 16) - gStart) / nbDivision;
            int bStart = Integer.parseInt(codeFrom.substring(4, 6), 16);
            int bDelta = (Integer.parseInt(codeTo.substring(4, 6), 16) - bStart) / nbDivision;
            for (int idx = 0;idx < nbDivision;idx++) {
                String red = Integer.toHexString(rStart + rDelta * idx);
                String green = Integer.toHexString(gStart + gDelta * idx);
                String blue = Integer.toHexString(bStart + bDelta * idx);
                colors.add(red + green + blue);
            }
        }
        return colors;
    }
    
    /**
     * Dedicated gradient for ff4j console (Pie Chart).
     *
     * @param nbsectors
     *      target sectors
     * @return
     *      color gradient
     */
    public static List < String > getColorsGradient(int nbsectors) {
        return getColorGradient(START_COLOR, END_COLOR, nbsectors);
    }
    
    /**
     * Allow to instanciate utility class.
     *
     * @param utilityClass
     *      current utility
     * @return
     *      instance
     * @throws Exception
     */
    public static <T> T instanciatePrivate(Class<T> utilityClass) throws Exception {
        Constructor<T> ce = utilityClass.getDeclaredConstructor();
        ce.setAccessible(true);
        return ce.newInstance();
    }
    
    /**
     * Get key listfrom value.
     *
     * @param map
     *      current MAP
     * @param value
     *      value of MAP
     * @return
     */
    public static <T, E> Set<T> getKeysByValue(Map<T, E> map, E value) {
        if (map == null) return null;
        Set<T> keys = new HashSet<T>();
        for (Entry<T, E> entry : map.entrySet()) {
            if (value != null && value.equals(entry.getValue())) {
                keys.add(entry.getKey());
            }
        }
        return keys;
    }
    
    /**
     * Get a first key matching from value.
     *
     * @param map
     *      current MAP
     * @param value
     *      value of MAP
     * @return
     */
    public static <T, E> T getFirstKeyByValue(Map<T, E> map, E value) {
        if (map == null) return null;
        for (Entry<T, E> entry : map.entrySet()) {
            if (value != null && value.equals(entry.getValue())) {
                return entry.getKey();
            }
        }
        return null;
    }

}
