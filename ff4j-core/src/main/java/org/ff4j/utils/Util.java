package org.ff4j.utils;

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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Tips and tricks to be less verbose.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class Util {

    /**
     * Remove default constructor.
     */
    private Util() {}

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
    public static void assertNotNull(Object object) {
        if (object == null) {
            throw new IllegalArgumentException("[Assertion failed] - this argument is required; it must not be null");
        }
    }

    /**
     * Check that string is not null
     * 
     * @param object
     *            target object
     */
    public static void assertHasLength(String text) {
        if (null != text && !text.isEmpty()) {
            throw new IllegalArgumentException(
                    "[Assertion failed] - this String argument must have length; it must not be null or empty");
        }
    }

    /**
     * Create an HashSet.
     *
     * @param els
     *            enumeration of elements
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> Set<T> hashSet(T... els) {
        return new HashSet<T>(Arrays.asList(els));
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
    public static List < String > getColorGradient(String codeFrom, String codeTo, int nbDivision) {
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
        return getColorGradient("00AB8B", "EEFFEE", nbsectors);
    }

}
