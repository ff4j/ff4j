package org.ff4j.property;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import static org.ff4j.test.AssertUtils.*;

import java.lang.reflect.Constructor;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.utils.Util;

/**
 * Create {@link Property} from name type and value.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyFactory {

    private static Map < Class<?> , Class<?> > validPropertyPrimitives = new HashMap<Class<?>, Class<?> >();
    
    /**
     * Initialized Primitive to work with Properties.
     */
    static {
        validPropertyPrimitives.put(Byte.class, PropertyByte.class);
        validPropertyPrimitives.put(Short.class, PropertyShort.class);
        validPropertyPrimitives.put(Integer.class, PropertyInt.class);
        validPropertyPrimitives.put(Long.class, PropertyLong.class);
        validPropertyPrimitives.put(Double.class, PropertyDouble.class);
        validPropertyPrimitives.put(Boolean.class, PropertyBoolean.class);
        validPropertyPrimitives.put(Float.class, PropertyFloat.class);
        validPropertyPrimitives.put(BigInteger.class, PropertyBigInteger.class);
        validPropertyPrimitives.put(BigDecimal.class, PropertyBigDecimal.class);
        validPropertyPrimitives.put(PropertyLogLevel.LogLevel.class, PropertyLogLevel.class);
        validPropertyPrimitives.put(String.class, PropertyString.class);
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
    public static Property<?> createProperty(String pName, Object value) {
        assertHasLength(pName);
        assertNotNull(value);
        if (validPropertyPrimitives.containsKey(value.getClass())) {
            return PropertyFactory.createProperty(pName, 
                    validPropertyPrimitives.get(value.getClass()).getName(), 
                    String.valueOf(value));
        }
        if (value instanceof Date) {
            return PropertyFactory.createProperty(pName, 
                    PropertyDate.class.getName(),
                    PropertyDate.SIMPLE_DATE_FORMAT.format(value));
        }
        if (value instanceof Calendar) {
            Date valueDate = ((Calendar) value).getTime();
            return PropertyFactory.createProperty(pName, 
                    PropertyCalendar.class.getName(),
                    PropertyCalendar.SIMPLE_DATE_FORMAT.format(valueDate));
        }
        if (value instanceof Property<?>) {
            return (Property<?>) value;
        }
        // String Value
        if (value.getClass().isArray() || Util.isCollection(value)) {
            return PropertyFactory.createProperty(pName, 
                    PropertyString.class.getName(), 
                    Util.join(Util.asCollection(value), ","));
        }
        throw new IllegalArgumentException("Cannot create property with input type "  + value.getClass() + value.toString());
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
    public static Property<?> createProperty(Property<?> from) {
        Property<?> to = PropertyFactory.createProperty(from.getUid(), 
                from.getClassName(), from.asString(), 
                from.getDescription().orElse(""), null);
        if (from.getFixedValues().isPresent()) {
            for (Object fixedValue : from.getFixedValues().get()) {
                to.add2FixedValueFromString(fixedValue.toString());
            }
        }
        return to;
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
        assertHasLength(pName);
        assertHasLength(pType);
        try {
            Constructor<?> constr = Class.forName(pType).getConstructor(String.class, String.class);
            final Property<?> ap = (Property<?>) constr.newInstance(pName, pValue);
            ap.setDescription(desc);
            ap.addFixedValues(fixedValues);
            return ap;
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot instantiate '" + pType + "' check default constructor : " + e.getMessage(), e);
        }
    }
}
