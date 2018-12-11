package org.ff4j.property;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2018 FF4J
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

import static org.ff4j.utils.Util.setOf;

import java.lang.reflect.ParameterizedType;
import java.text.SimpleDateFormat;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;

import org.ff4j.FF4jEntity;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.Util;

/**
 * Abstraction of Property.
 *
 * @author Cedrick Lunven (@clunven)
 */
public abstract class Property<T> extends FF4jEntity<Property<T>> implements Supplier<T> {

    /** serialVersionUID. */
    private static final long serialVersionUID = -2484426537747694712L;
    
    /** Date time format. */
    public static final String DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";
    
    /** expected expression. */
    public  static final SimpleDateFormat SIMPLE_DATE_FORMAT = new SimpleDateFormat(DATE_FORMAT);
    
    /** formatter for creation date and last modified. */
    public static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern(DATE_FORMAT);
    
    /** zone offset. */
    public static ZoneOffset ZONE = ZoneOffset.UTC;
    
    /** Mapping of some property. */
    protected static Map < String, String > PROPERTY_TYPES;
    
    /** Type of property. */
    protected String className;

    /** Current Value. */
    protected T value;

    /** If value have a limited set of values. */
    protected Set<T> fixedValues = null;
    
    /**
     * Constructor by property name.
     *
     * @param name
     *            unique property name
     */
    protected Property(String uid) {
        super(uid);
        this.className = getClass().getName();
    }

    /**
     * Constructor with name and value as String.
     *
     * @param name
     *            current name
     * @param value
     *            current value
     */
    protected Property(String name, String value) {
        this(name);
        this.value = fromString(value);
    }

    /**
     * Constructor with name and value as String.
     *
     * @param name
     *            current name
     * @param value
     *            current value
     */
    protected Property(String name, T value) {
        this(name);
        this.value = value;
    }
    
    public Property(Property<T> e) {
        this(e.getUid(), e);
    }
        
    public Property(String uid, Property<T> e) {
        super(uid, e);
        this.className     = e.getClassName();
        this.value    = e.getValue();
        if (e.getFixedValues().isPresent()) {
            for(T fixedValue : e.getFixedValues().get()) {
                add2FixedValueFromString(fixedValue.toString());
            }
        }
    }

    /**
     * Check dynamically the class of the parameter T.
     *
     * @return class of template T parameter
     * @throws Exception
     *             error on reading type
     */
    @SuppressWarnings({"unchecked"})
    public Class<T> parameterizedType() {
        ParameterizedType pt = (ParameterizedType) getClass().getGenericSuperclass();
        return (Class<T>) pt.getActualTypeArguments()[0];
    }
    
    /**
     * Initialisation of substitution types
     */
    static {
        PROPERTY_TYPES = new HashMap<String, String >();
        PROPERTY_TYPES.put("bigDecimal", PropertyBigDecimal.class.getName());
        PROPERTY_TYPES.put("bigInteger", PropertyBigInteger.class.getName());
        PROPERTY_TYPES.put("boolean",    PropertyBoolean.class.getName());
        PROPERTY_TYPES.put("byte",       PropertyByte.class.getName());
        PROPERTY_TYPES.put("calendar",   PropertyCalendar.class.getName());
        PROPERTY_TYPES.put("class",      PropertyClass.class.getName());
        PROPERTY_TYPES.put("date",       PropertyDate.class.getName());
        PROPERTY_TYPES.put("double",     PropertyDouble.class.getName());
        PROPERTY_TYPES.put("float",      PropertyFloat.class.getName());
        PROPERTY_TYPES.put("instant",    PropertyInstant.class.getName());
        PROPERTY_TYPES.put("int",        PropertyInt.class.getName());
        PROPERTY_TYPES.put("localDateTime",  PropertyLocalDateTime.class.getName());
        PROPERTY_TYPES.put("logLevel",   PropertyLogLevel.class.getName());
        PROPERTY_TYPES.put("long",       PropertyLong.class.getName());
        PROPERTY_TYPES.put("short",      PropertyShort.class.getName());
        PROPERTY_TYPES.put("string",     PropertyString.class.getName());
        
        PROPERTY_TYPES.put("listBigDecimal", PropertyListBigDecimal.class.getName());
        PROPERTY_TYPES.put("listBigInteger", PropertyListBigInteger.class.getName());
        PROPERTY_TYPES.put("listBoolean",    PropertyListBoolean.class.getName());
        PROPERTY_TYPES.put("listByte",       PropertyListByte.class.getName());
        PROPERTY_TYPES.put("listCalendar",   PropertyListCalendar.class.getName());
        PROPERTY_TYPES.put("listClass",      PropertyListClass.class.getName());
        PROPERTY_TYPES.put("listDate",       PropertyListDate.class.getName());
        PROPERTY_TYPES.put("listDouble",     PropertyListDouble.class.getName());
        PROPERTY_TYPES.put("listFloat",      PropertyListFloat.class.getName());
        PROPERTY_TYPES.put("listInstant",    PropertyListInstant.class.getName());
        PROPERTY_TYPES.put("listInt",        PropertyListInt.class.getName());
        PROPERTY_TYPES.put("listLocalDateTime",  PropertyListLocalDateTime.class.getName());
        PROPERTY_TYPES.put("listLogLevel",   PropertyListLogLevel.class.getName());
        PROPERTY_TYPES.put("listLong",       PropertyListLong.class.getName());
        PROPERTY_TYPES.put("listShort",      PropertyListShort.class.getName());
        PROPERTY_TYPES.put("listString",     PropertyListString.class.getName());
    }
    
    /**
     * Substitution of primitive into PropertyXXX.
     *
     * @param pType
     * @return
     */
    public static String mapPropertyType(String pType) {
        if (pType == null) return null;
        if (PROPERTY_TYPES.containsKey(pType)) {
           return PROPERTY_TYPES.get(pType);
        }
        return pType;
    }
    
    /**
     * Substitution of PropertyXXX intoPrimitive.
     *
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
     *
     * @param pType
     * @return
     */
    public static String mapSimpleType(Class<?> pType) {
        if (pType == null) return null;
        return mapSimpleType(pType.getName());
    }

    /**
     * Unmarshalling of value for serailized string expression.
     *
     * @param v
     *            value represented as a serialized String
     * @return target value
     */
    public abstract T fromString(String v);
    
    /** {@inheritDoc} */
    @Override
    public T get() {
        return value;
    }
    
    /** {@inheritDoc} */
    public T getValue() {
       return get();
    }
    
    /**
     * Serialized value as String
     *
     * @return current value as a string or null
     */
    public String asString() {
        if (get() == null) {
            return null;
        }
        return get().toString();
    }

    /**
     * Return value as int (if possible).
     *
     * @return int value
     */
    public int asInt() {
        return Integer.parseInt(asString());
    }

    /**
     * Return value as double if possible.
     *
     * @return int value
     */
    public double asDouble() {
        return Double.parseDouble(asString());
    }

    /**
     * Return value as boolean if possible.
     *
     * @return boolea value
     */
    public boolean asBoolean() {
        return Boolean.parseBoolean(asString());
    }
    
    /**
     * Setter accessor for attribute 'value'.
     * 
     * @param value
     *            new value for 'value '
     */
    public Property<T> setValue(T value) {
        if (fixedValues != null && !fixedValues.isEmpty() && !fixedValues.contains(value)) {
            throw new IllegalArgumentException("Invalid value corrects are " + fixedValues);
        }
        this.value = value;
        return this;
    }

    /**
     * Load value from its string expression.
     *
     * @param value
     *            current string value
     */
    public Property<T> setValueFromString(String value) {
        this.value = fromString(value);
        return this;
    }

    /**
     * Getter accessor for attribute 'fixedValues'.
     *
     * @return current value of 'fixedValues'
     */
    public Optional<Set<T>> getFixedValues() {
        return Optional.ofNullable(fixedValues);
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }

    /** {@inheritDoc} */
    public String toJson() {
        StringBuilder jsonExpression = new StringBuilder("{ ");
        jsonExpression.append(super.baseJson());
        jsonExpression.append(",\"type\":\"" + className + "\"");
        jsonExpression.append(",\"value\":");
        jsonExpression.append((null == value) ? "null" : "\"" + asString() + "\"");
        if (fixedValues != null) {
            jsonExpression.append(",\"fixedValues\":" + JsonUtils.collectionAsJson(fixedValues));
        }
        jsonExpression.append("}");
        return jsonExpression.toString();
    }
    
    public void addFixedValues(Set<String> fixedValues) {
        if (fixedValues != null) {
            fixedValues.stream().forEach(v -> add2FixedValueFromString(v.trim()));
            if (!fixedValues.contains(getValue())) {
                throw new IllegalArgumentException("Cannot create property <" + getUid() + "> invalid value <"
                            + getValue() + "> expected one of " + getFixedValues());
            }
        }
    }
    
    @SuppressWarnings("unchecked")
    public Property<T> setFixedValues(T... perms) {
        return setFixedValues(setOf(perms));
    }

    public Property<T> setFixedValues(Set<T> perms) {
        fixedValues = perms;
        return this;
    }

    public Property<T> add2FixedValueFromString(String v) {
        return addFixedValue(fromString(v));
    }

    @SuppressWarnings("unchecked")
    public Property<T> addFixedValue(T permission) {
        return addFixedValues(permission);
    }

    @SuppressWarnings("unchecked")
    public Property<T> addFixedValues(T... fixed) {
        if (fixed != null) {
            if (fixedValues == null) {
                fixedValues = new HashSet<>();
            }
            fixedValues.addAll(setOf(fixed));
        }
        return this;
    }
    
    /**
     * Getter accessor for attribute 'type'.
     *
     * @return
     *       current value of 'type'
     */
    public String getClassName() {
        return className;
    }

    /**
     * Setter accessor for attribute 'type'.
     * @param type
     * 		new value for 'type '
     */
    public void setClassName(String type) {
        this.className = type;
    }

}
