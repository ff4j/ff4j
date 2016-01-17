package org.ff4j.property.util;

/*
 * #%L
 * ff4j-core
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
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;
import java.util.Set;

import org.ff4j.property.Property;
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
import org.ff4j.property.PropertyShort;
import org.ff4j.utils.Util;

/**
 * Create {@link Property} from name type and value.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyFactory {

    /**
     * Hide constructor as util class.
     */
    public PropertyFactory() {}

    /**
     * Factory method to create property.
     *
     * @param pName
     *            property name.
     * @param pType
     *            property type
     * @param pValue
     *            property value
     * @return
     */
    public static Property<?> createProperty(String pName, Object value) {
        Util.assertHasLength(pName);
        Util.assertNotNull(value);
        if (value instanceof Byte) {
            return PropertyFactory.createProperty(pName, PropertyByte.class.getName(), String.valueOf(value), null, null);
        }
        if (value instanceof Short) {
            return PropertyFactory.createProperty(pName, PropertyShort.class.getName(), String.valueOf(value), null, null);
        }
        if (value instanceof Integer) {
            return PropertyFactory.createProperty(pName, PropertyInt.class.getName(), String.valueOf(value), null, null);
        }
        if (value instanceof Long) {
            return PropertyFactory.createProperty(pName, PropertyInt.class.getName(), String.valueOf(value), null, null);
        }
        if (value instanceof Double) {
            return PropertyFactory.createProperty(pName, PropertyDouble.class.getName(), String.valueOf(value), null, null);
        }
        if (value instanceof Boolean) {
            return PropertyFactory.createProperty(pName, PropertyBoolean.class.getName(), String.valueOf(value), null, null);
        }
        if (value instanceof Float) {
            return PropertyFactory.createProperty(pName, PropertyFloat.class.getName(), String.valueOf(value), null, null);
        }
        if (value instanceof BigInteger) {
            return PropertyFactory.createProperty(pName, PropertyBigInteger.class.getName(), String.valueOf(value), null, null);
        }
        if (value instanceof BigDecimal) {
            return PropertyFactory.createProperty(pName, PropertyBigDecimal.class.getName(), String.valueOf(value), null, null);
        }
        if (value instanceof String) {
            return PropertyFactory.createProperty(pName, PropertyString.class.getName(), String.valueOf(value), null, null);
        }
        if (value instanceof PropertyLogLevel.LogLevel) {
            return PropertyFactory.createProperty(pName, PropertyLogLevel.class.getName(), String.valueOf(value), null, null);
        }
        if (value instanceof Date) {
            return PropertyFactory.createProperty(pName, PropertyDate.class.getName(), PropertyDate.SDF.format(value), null, null);
        }
        if (value instanceof Calendar) {
            Date valueDate = ((Calendar) value).getTime();
            return PropertyFactory.createProperty(pName, 
                    PropertyCalendar.class.getName(),
                    PropertyCalendar.SDF.format(valueDate), null, null);
        }
        if (value instanceof Property<?>) {
            return (Property<?>) value;
        }
        throw new IllegalArgumentException("Cannot create property with input type "  + value.getClass());
    }
    
    /**
     * Factory method to create property.
     *
     * @param pName
     *            property name.
     * @param pType
     *            property type
     * @param pValue
     *            property value
     * @return
     */
    public static Property<?> createProperty(String pName, String pType, String pValue) {
        return PropertyFactory.createProperty(pName, pType, pValue, null, null);
    }

    /**
     * Create Property from generic bean.
     *
     * @param pgb
     *           generic bean
     * @return
     */
    public static Property<?> createProperty(PropertyJsonBean pgb) {
        return PropertyFactory.createProperty(
                pgb.getName(), pgb.getType(), 
                pgb.getValue(), pgb.getDescription(), 
                pgb.getFixedValues());
    }

    /**
     * Factory method to create property.
     *
     * @param pName
     *            property name.
     * @param pType
     *            property type
     * @param pValue
     *            property value
     * @return
     */
    public static Property<?> createProperty(String pName, String pType, String pValue, String desc, Set < String > fixedValues) {
        Util.assertNotNull(pName);
        Util.assertNotNull(pType);
        Property<?> ap = null;
        try {
            Constructor<?> constr = Class.forName(pType).getConstructor(String.class, String.class);
            ap = (Property<?>) constr.newInstance(pName, pValue);
            ap.setDescription(desc);
            // Is there any fixed Value ?
            if (fixedValues != null && !fixedValues.isEmpty()) {
                for (String v : fixedValues) {
                    ap.add2FixedValueFromString(v.trim());
                }
                // Should be filled before test
                if (!ap.getFixedValues().contains(ap.getValue())) {
                    throw new IllegalArgumentException("Cannot create property <" + ap.getName() + "> invalid value <"
                                + ap.getValue() + "> expected one of " + ap.getFixedValues());
                }
            }
            return ap;
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot instantiate '" + pType + "' check default constructor : " + e.getMessage(), e);
        }
    }
}
