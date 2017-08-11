package org.ff4j.property;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.ff4j.property.util.PropertyJsonBean;
import org.ff4j.utils.Util;

/**
 * Abstraction of Property.
 *
 * @author Cedrick Lunven (@clunven)
 */
public abstract class Property < T > implements Serializable {
    
    /** serial. */
    private static final long serialVersionUID = 4987351300418126366L;
    
    /** Some store do not allow property edition. */
    protected boolean readOnly = false;

    /** Unique name for property. */
    protected String name;
    
    /** Short description of the property. */
    protected String description = null;
    
    /** Canonical name for JSON serialization. */
    protected String type = getClass().getName();
    
    /** Current Value. */
    protected T value;
    
    /** If value have a limited set of values. */
    protected Set < T > fixedValues;
    
    /**
     * Default constructor.
     */
    protected Property() {
    }
            
    /**
     * Constructor by property name.
     *
     * @param name
     *         unique property name
     */
    protected Property(String name) {
        Util.assertHasLength(name);
        this.name = name;
    }
   
    /**
     * Constructor with name and value as String.
     *
     * @param name
     *      current name
     * @param value
     *      current value
     */
    protected Property(String name, String value) {
        this(name);
        this.value = fromString(value);
    }
    
	/**
     * Constructor with name , value and target available values
     *
     * @param name
     *      current name
     * @param value
     *      current value
     */    
    protected Property(String name, T value, Set < T > fixed) {
        this(name);
        this.value = value;
        this.fixedValues = fixed;
        if (fixedValues != null &&  !fixedValues.isEmpty() && !fixedValues.contains(value)) {
            throw new IllegalArgumentException("Invalid value corrects are " + fixedValues);
        }
    }
    
    /**
     * Constructor with name , value and target available values
     *
     * @param name
     *      current name
     * @param value
     *      current value
     */
    @SafeVarargs
    protected Property(String name, T value, T... fixed) {
        this(name, value, new HashSet<T>(Arrays.asList(fixed)));
    }
    
    /**
     * Help XML parsing to realize downcastings.
     *
     * @param v
     *      current value as String
     */
    public void add2FixedValueFromString(String v) {
        add2FixedValue(fromString(v));
    }
    
    /**
     * Add element to fixed values.
     * 
     * @param value
     *      current value
     */
    public void add2FixedValue(T value) {
        if (fixedValues == null) {
            fixedValues = new HashSet<T>();
        }
        fixedValues.add(value);
    }
    
    /**
     * Unmarshalling of value for serailized string expression.
     *
     * @param v
     *      value represented as a serialized String
     * @return
     *      target value
     */
    public abstract T fromString(String v);
    
    /**
     * Check dynamically the class of the parameter T.
     *
     * @return
     *      class of template T parameter
     * @throws Exception
     *      error on reading type
     */
    @SuppressWarnings({ "unchecked" })
    public Class<T> parameterizedType() {
        ParameterizedType pt = (ParameterizedType) getClass().getGenericSuperclass();
        return  (Class<T>) pt.getActualTypeArguments()[0];
    }
    
    /** 
     * Serialized value as String
     *
     * @return
     *      current value as a string or null
     */
    public String asString() {
        if (value == null) {
            return null;
        }
        return value.toString();
    }
    
    /**
     * Return value as int (if possible).
     *
     * @return
     *      int value
     */
    public int asInt() {
        return Integer.parseInt(asString());
    }

    /**
     * Return value as double if possible.
     *
     * @return
     *      int value
     */
    public double asDouble() {
        return Double.parseDouble(asString());
    }
    
    /**
     * Return value as boolean if possible.
     *
     * @return
     *      boolea value
     */
    public boolean asBoolean() {
        return Boolean.parseBoolean(asString());
    }

    /**
     * Getter accessor for attribute 'value'.
     *
     * @return
     *       current value of 'value'
     */
    public T getValue() {
        return value;
    }

    /**
     * Setter accessor for attribute 'value'.
     * @param value
     * 		new value for 'value '
     */
    public void setValue(T value) {
        this.value = value;
    }
    
    /**
     * Load value from its string expression.
     *
     * @param value
     *      current string value
     */
    public void setValueFromString(String value) {
        this.value = fromString(value);
    }

    /**
     * Getter accessor for attribute 'name'.
     *
     * @return
     *       current value of 'name'
     */
    public String getName() {
        return name;
    }

    /**
     * Setter accessor for attribute 'name'.
     * @param name
     * 		new value for 'name '
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Getter accessor for attribute 'fixedValues'.
     *
     * @return
     *       current value of 'fixedValues'
     */
    public Set<T> getFixedValues() {
        return fixedValues;
    }

    /**
     * Setter accessor for attribute 'fixedValues'.
     * @param fixedValues
     * 		new value for 'fixedValues '
     */
    public void setFixedValues(Set<T> fixedValues) {
        this.fixedValues = fixedValues;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }
    
    /** {@inheritDoc} */
    public String toJson() {
        return new PropertyJsonBean(this).asJson();
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

    /**
     * Getter accessor for attribute 'description'.
     *
     * @return
     *       current value of 'description'
     */
    public String getDescription() {
        return description;
    }

    /**
     * Setter accessor for attribute 'description'.
     * @param description
     * 		new value for 'description '
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Getter accessor for attribute 'readOnly'.
     *
     * @return
     *       current value of 'readOnly'
     */
    public boolean isReadOnly() {
        return readOnly;
    }

    /**
     * Setter accessor for attribute 'readOnly'.
     * @param readOnly
     * 		new value for 'readOnly '
     */
    public void setReadOnly(boolean readOnly) {
        this.readOnly = readOnly;
    }

}
