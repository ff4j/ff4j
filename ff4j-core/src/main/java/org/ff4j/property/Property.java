package org.ff4j.property;

import static org.ff4j.test.AssertUtils.assertNotNull;
/*
 * #%L ff4j-core %% Copyright (C) 2013 - 2016 FF4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */
import static org.ff4j.utils.Util.setOf;

import java.lang.reflect.ParameterizedType;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;

import org.ff4j.FF4jEntity;
import org.ff4j.property.domain.PropertyBigDecimal;
import org.ff4j.property.domain.PropertyBigInteger;
import org.ff4j.property.domain.PropertyBoolean;
import org.ff4j.property.domain.PropertyByte;
import org.ff4j.property.domain.PropertyCalendar;
import org.ff4j.property.domain.PropertyDate;
import org.ff4j.property.domain.PropertyDouble;
import org.ff4j.property.domain.PropertyFloat;
import org.ff4j.property.domain.PropertyInt;
import org.ff4j.property.domain.PropertyLogLevel;
import org.ff4j.property.domain.PropertyLong;
import org.ff4j.property.domain.PropertyShort;
import org.ff4j.property.domain.PropertyString;
import org.ff4j.FF4jContext;
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
    
    /** Mapping of some property. */
    private static Map < String, String > PROPERTY_TYPES;
    
    /** Type of property. */
    protected String type;

    /** Current Value. */
    protected T value;

    /** Some store do not allow property edition. */
    protected boolean readOnly = false;

    /** If value have a limited set of values. */
    protected Set<T> fixedValues = null;

    /** Can compute the property value based on your own implementation. */
    protected Optional < PropertyEvaluationStrategy<T> > evaluationStrategy = Optional.empty();

    /**
     * Constructor by property name.
     *
     * @param name
     *            unique property name
     */
    protected Property(String uid) {
        super(uid);
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
        this.readOnly = e.isReadOnly();
        this.type     = e.getType();
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
     *
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
        return getValue();
    }
    
    /** {@inheritDoc} */
    public T get(FF4jContext ctx) {
        return getValue(ctx);
    }
    
    /** {@inheritDoc} */
    public T getValue() {
        // no context
       return getValue(null);
    }

    /**
     * If an execution is provided evaluate the property value.
     *
     * @param pec
     *            evaluation strategy
     * @return property value
     */
    public T getValue(FF4jContext ctx) {
        if (!evaluationStrategy.isPresent()) return value;
        return evaluationStrategy.get().getValue(this, ctx);
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
        jsonExpression.append(",\"type\":\"" + type + "\"");
        jsonExpression.append(",\"readOnly\":\"" + readOnly + "\"");
        jsonExpression.append(",\"value\":");
        jsonExpression.append((null == value) ? "null" : "\"" + asString() + "\"");
        if (fixedValues != null) {
            jsonExpression.append(",\"fixedValues\":" + JsonUtils.collectionAsJson(fixedValues));
        }
        jsonExpression.append("}");
        return jsonExpression.toString();
    }
    
    public void addFixedValues(Set<String> fixedValues) {
        assertNotNull(fixedValues);
        fixedValues.stream().forEach(v -> add2FixedValueFromString(v.trim()));
        if (!fixedValues.contains(getValue())) {
            throw new IllegalArgumentException("Cannot create property <" + getUid() + "> invalid value <"
                        + getValue() + "> expected one of " + getFixedValues());
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
     * Getter accessor for attribute 'readOnly'.
     *
     * @return current value of 'readOnly'
     */
    public boolean isReadOnly() {
        return readOnly;
    }

    /**
     * Setter accessor for attribute 'readOnly'.
     * 
     * @param readOnly
     *            new value for 'readOnly '
     */
    public Property<T> setReadOnly(boolean readOnly) {
        this.readOnly = readOnly;
        return this;
    }

    /**
     * Getter accessor for attribute 'evaluationStrategy'.
     *
     * @return
     *       current value of 'evaluationStrategy'
     */
    public Optional < PropertyEvaluationStrategy<T> > getEvaluationStrategy() {
        return evaluationStrategy;
    }

    /**
     * Setter accessor for attribute 'evaluationStrategy'.
     * @param evaluationStrategy
     * 		new value for 'evaluationStrategy '
     */
    public void setEvaluationStrategy(PropertyEvaluationStrategy<T> evaluationStrategy) {
        this.evaluationStrategy = Optional.ofNullable(evaluationStrategy);
    }

    /**
     * Getter accessor for attribute 'type'.
     *
     * @return
     *       current value of 'type'
     */
    public String getType() {
        return type;
    }

    /**
     * Setter accessor for attribute 'type'.
     * @param type
     * 		new value for 'type '
     */
    public void setType(String type) {
        this.type = type;
    }

}
