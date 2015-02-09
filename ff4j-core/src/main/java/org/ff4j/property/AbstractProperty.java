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

import java.util.List;

import org.ff4j.utils.Util;

/**
 * Property to be store
 *
 * { name="", type="INT", value="", fixedValue="" }
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class AbstractProperty < T > {
    
    /** Unique identifier. */
    protected String name;
    
    /** Typed value. */
    protected T value;
    
    /** Close list of value. */
    protected List < T > fixedValues;
    
    /**
     * Default constructor.
     *
     * @param json
     */
    public AbstractProperty() {
    }
            
    /**
     * Default constructor.
     *
     * @param json
     */
    public AbstractProperty(String name) {
        Util.assertHasLength(name);
        this.name = name;
    }
            
    /**
     * Init from String.
     *
     * @param json
     */
    public AbstractProperty(String name, String value) {
        this(name);
        this.value = fromString(value);
    }
    
    /**
     * Param JSON.
     *
     * @param json
     */
    public AbstractProperty(String name, T value, List <T> fixed) {
        this(name);
        this.value = value;
        this.fixedValues = fixed;
        if (fixedValues!= null && !fixedValues.contains(value)) {
            throw new IllegalArgumentException("Invalid value corrects are " + fixedValues);
        }
    }
    
    /**
     * To be implemented if dedicated implementation.
     *
     * @param v
     *      current value
     * @return
     *      target objet
     */
    public abstract T fromString(String v);
    
    /** 
     * Handle Values.
     *
     * @return
     *      targate values
     */
    public String asString() {
        if (value == null) {
            return null;
        }
        return value.toString();
    }
    
    /**
     * Return value as int if possible.
     *
     * @return
     *      int value
     */
    public int asInt() {
        return new Integer(asString()).intValue();
    }

    /**
     * Return value as double if possible.
     *
     * @return
     *      int value
     */
    public double asDouble() {
        return new Double(asString()).doubleValue();
    }
    
    /**
     * Return value as boolean if possible.
     *
     * @return
     *      boolea value
     */
    public boolean asBoolean() {
        return new Boolean(asString()).booleanValue();
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
    public List<T> getFixedValues() {
        return fixedValues;
    }

    /**
     * Setter accessor for attribute 'fixedValues'.
     * @param fixedValues
     * 		new value for 'fixedValues '
     */
    public void setFixedValues(List<T> fixedValues) {
        this.fixedValues = fixedValues;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        String start = "{\"name\":\"" + name + "\",\"value\":\"" + value + "\",\"fixedValues\":";
        if (fixedValues == null) {
            start+="null";
        } else {
            start += "[";
            boolean first = true;
            for (T fv : fixedValues) {
                start += first ? "" : ",";
                start += "\"" + fv.toString() + "\"";
                first = false;
            }
            start += "]";
        }
        start += "}";
        return start;
    }

}
