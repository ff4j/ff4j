package org.ff4j.utils;

import java.awt.Color;
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

import javax.lang.model.type.NullType;

import java.util.Set;

import org.ff4j.audit.Event;

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
     * Check that class is valid.
     * 
     * @param expression
     *            expression to evaluate
     */
    public static boolean isValidClass(Class<?> clazz) {
        return (clazz != null) && (clazz != NullType.class);
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
    
    public static void assertEvent(Event evt) {
        assertNotNull(evt);
        assertHasLength(evt.getName());
        assertHasLength(evt.getType());
        assertHasLength(evt.getAction());
    }

    /**
     * Check that object is not null.
     * 
     * @param object
     *            target object
     */
    public static void assertNotNull(Object... params) {
        assertNotNull("parameter", params);
    }
    
    /**
     * Check that object is not null.
     * 
     * @param object
     *            target object
     */
    public static void assertNotNull(String objectName, Object... params) {
        if (params == null) {
            throw new IllegalArgumentException("[Assertion failed] - " + objectName + " must not be null");
        }
        for (int idx = 0; idx < params.length ;idx++) {
            Object currentparam = params[idx];
            if (null == currentparam) {
                throw new IllegalArgumentException("[Assertion failed] - " + objectName + " must not be null");
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
        if (params == null) {
            throw new IllegalArgumentException("[Assertion failed] - Parameter #0 (string)  must not be null nor empty");
        }
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
    public static void assertParamHasLength(String param, String paramName) {
        if (param == null || param.isEmpty()) {
            throw new IllegalArgumentException("Missing Parameter '" + paramName + "' must not be null nor empty");
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
    public static void assertParamHasNotNull(Object param, String paramName) {
        if (param == null) {
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
	@SafeVarargs
    public static <T> Set<T> set(T... els) {
	    if (els == null) return null;
        return new HashSet<T>(Arrays.asList(els));
    }
	
	/**
     * Create an HashSet.
     *
     * @param els
     *            enumeration of elements
     * @return
     */
	@SafeVarargs
    public static <T> List<T> list(T... els) {
        if (els == null) return null;
        return new ArrayList<T>(Arrays.asList(els));
    }
    
    /**
     * Serialize collection elements with a delimiter.
     *
     * @param collec
     *      collection (a,b,c)
     * @param delimiter
     *      delimiter char (e.g : ",")
     * @return
     *      the list : a,b,c
     */
    public static <T> String join(Collection < T > collec, String delimiter) {
        assertNotNull(delimiter);
        if (collec == null) return null;
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (T t : collec) {
            if (!first) {
                sb.append(",");
            }
            sb.append(t.toString());
            first = false;
        }
        return sb.toString();
    }
    
    /**
     * Check if a current class can be cast to collection.
     * 
     * @param c
     *      current class
     * @return
     *      flag if it's a collection
     */
    public static boolean isClassCollection(Class<?> c) {
        return Collection.class.isAssignableFrom(c) || Map.class.isAssignableFrom(c);
    }
    
    /**
     * Check if a current object can be cast to collection.
     * 
     * @param ob
     *      current object
     * @return
     *      flag if it's a collection
     */
    public static boolean isCollection(Object ob) {
        return ob != null && isClassCollection(ob.getClass());
    }
    
    /**
     * Check if a current object is empty
     * 
     * @param ob
     *      current collection
     * @return
     *      flag if it's an empty collection
     */
    public static boolean isEmpty(Collection<?> ob) {
        return (ob == null) || ob.isEmpty();
    }
    
    /**
     * Downcast as collection or return error.
     *
     * @param ob
     *      target object
     * @return
     *      if can ve converted to collection.
     */
    @SuppressWarnings("unchecked")
    public static <T> Collection < T > asCollection(Object ob) {
        if (ob == null) return null;
        if (ob.getClass().isArray()) {
            return Arrays.asList((T[]) ob);
        }
        if (!isCollection(ob)) {
            throw new IllegalArgumentException("Target Object is not collection");
        }
        return (Collection<T>) ob;
    }
    
    /**
     * Get a random offset within map.
     *
     * @param size
     *      target list size
     * @return
     *      a random positive integer below size
     */
    public static int getRandomOffset(int size) {
        return (int) (Math.random() * Math.abs(size));
    }
    
    /**
     * Get a random element from a list.
     *
     * @param myList
     *      current list
     */
    public static < T > T getRandomElement(List<T> myList) {
        return myList.get(getRandomOffset(myList.size()));
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
    public static List < String > generateRGBGradient(String codeFrom, String codeTo, int nbDivision) {
        List < String > colors = new ArrayList<String>();
        if (nbDivision < 1) {
           nbDivision = 1;
        }
        nbDivision++;
        int r1 = Integer.parseInt(codeFrom.substring(0, 2), 16);
        int g1 = Integer.parseInt(codeFrom.substring(2, 4), 16);
        int b1 = Integer.parseInt(codeFrom.substring(4, 6), 16);
        int r2 = Integer.parseInt(codeTo.substring(0, 2), 16);
        int g2 = Integer.parseInt(codeTo.substring(2, 4), 16);
        int b2 = Integer.parseInt(codeTo.substring(4, 6), 16);
        int rDelta = (r2 - r1) / nbDivision;
        int gDelta = (g2 - g1) / nbDivision;
        int bDelta = (b2 - b1) / nbDivision;
        for (int idx = 0;idx < nbDivision;idx++) {
            String red   = Integer.toHexString(r1 + rDelta * idx);
            String green = Integer.toHexString(g1 + gDelta * idx);
            String blue  = Integer.toHexString(b1 + bDelta * idx);
            colors.add(red + green + blue);
        }
        return colors.subList(1, colors.size());
    }
    
    public static List < String > generateHSVGradient(String codeFrom, String codeTo, int nbDivision) {
        int r1 = Integer.parseInt(codeFrom.substring(0, 2), 16);
        int g1 = Integer.parseInt(codeFrom.substring(2, 4), 16);
        int b1 = Integer.parseInt(codeFrom.substring(4, 6), 16);
        int r2 = Integer.parseInt(codeTo.substring(0, 2), 16);
        int g2 = Integer.parseInt(codeTo.substring(2, 4), 16);
        int b2 = Integer.parseInt(codeTo.substring(4, 6), 16);
        float[] startHSB = Color.RGBtoHSB(r1, g1, b1, null);
        float[] endHSB   = Color.RGBtoHSB(r2, g2, b2, null);
        float brightness = (startHSB[2] + endHSB[2]) / 2;
        float saturation = (startHSB[1] + endHSB[1]) / 2;
        float hueMax = 0;
        float hueMin = 0;
        if (startHSB[0] > endHSB[0]) {
            hueMax = startHSB[0];
            hueMin = endHSB[0];
        } else {
            hueMin = startHSB[0];
            hueMax = endHSB[0];
        }
        List < String > colors = new ArrayList<String>();
        for (int idx = 0;idx < nbDivision;idx++) {
            float hue = ((hueMax - hueMin) * idx/nbDivision) + hueMin;
            int rgbColor = Color.HSBtoRGB(hue, saturation, brightness);
            colors.add(Integer.toHexString(rgbColor).substring(2));
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
        return generateRGBGradient(START_COLOR, END_COLOR, nbsectors);
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
