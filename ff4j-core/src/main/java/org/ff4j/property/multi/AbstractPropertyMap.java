package org.ff4j.property.multi;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.property.Property;

/**
 * 
 * @author Cedrick Lunven (@clunven)</a>
 *
 * @param <T>
 *      myultliva
 * @param <M>
 */
public abstract class AbstractPropertyMap < T, M extends Map<String, ? extends T>> extends Property < M > implements Map< String, T > {

    /** serial. */
    private static final long serialVersionUID = 2612494170643655559L;
    
    /**
     * Default constructor.
     */
    public AbstractPropertyMap() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    @SuppressWarnings("unchecked")
    public AbstractPropertyMap(String name) {
        super(name);
        value = (M) new HashMap<String, T>();
    }
   
    /**
     * Constructor by T expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    @SuppressWarnings("unchecked")
    public AbstractPropertyMap(String uid, M value) {
        super(uid, value);
    }

    /**
     * Access Internal value.
     *
     * @return
     *      target value.
     */
    protected M value() {
        if (value == null) {
            throw new IllegalStateException("Value should not be null nor empty");
        }
        return value;
    }

    /** {@inheritDoc} */
    public int size() {
        return value().size();
    }

    /** {@inheritDoc} */
    public boolean isEmpty() {
        return value().isEmpty();
    }

    /** {@inheritDoc} */
    public boolean containsKey(Object key) {
        return value().containsKey(key);
    }

    /** {@inheritDoc} */
    public boolean containsValue(Object obj) {
        return value().containsValue(obj);
    }

    /** {@inheritDoc} */
    public T get(Object key) {
        return value().get(key);
    }   

    /** {@inheritDoc} */
    public T remove(Object key) {
        return value().remove(key);
    }

    /** {@inheritDoc} */
    public void clear() {
        value().clear();
    }

    /** {@inheritDoc} */
    public Set<String> keySet() {
        return  value().keySet();
    }

}
