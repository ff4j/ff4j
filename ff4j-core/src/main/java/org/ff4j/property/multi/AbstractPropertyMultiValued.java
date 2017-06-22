package org.ff4j.property.multi;

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


import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;

import org.ff4j.property.Property;

/**
 * Super class to work with multivalued properties.
 *
 * @author Cedrick Lunven (@clunven)
 *
 * @param <T>
 *      current inner type
 * @param <C>
 *      current collection type
 */
public abstract class AbstractPropertyMultiValued < T, C extends Collection< T >> extends Property < C > implements Collection< T > {

    /** Serial. */
    private static final long serialVersionUID = 1L;
    
    /** Delimiter for lists. */
    protected static final String DEFAULT_DELIMETER = ",";
    
    /** required if should be splip. */
    private String listDelimiter = DEFAULT_DELIMETER;
    
    /**
     * Default constructor.
     */
    public AbstractPropertyMultiValued() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public AbstractPropertyMultiValued(String name) {
        super(name);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    @SuppressWarnings("unchecked")
    public AbstractPropertyMultiValued(String uid, C value) {
       super(uid, value);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    public AbstractPropertyMultiValued(String uid, String value) {
       super(uid, value);
    }
    
    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    public C fromString(String v) {
        if (v == null) return null;
        String[] items = v.split(getListDelimiter());
        return (C) Arrays.asList(items);
    }
    
    /**
     * Add element to the collection.
     *
     * @param e
     *      new element
     */
    public boolean add(T e) {
        return (null != getValue()) ? getValue().add(e) : false;
    }
    
    /**
     * Add values to target collections.
     *
     * @param values
     *      target values
     */
    @SafeVarargs
    public final void addAll(T... values) {
        if (null != values) {
            getValue().addAll(Arrays.asList(values));
        }
    }
   
    /**
     * Getter accessor for attribute 'separator'.
     *
     * @return
     *       current value of 'separator'
     */
    public String getListDelimiter() {
        if (null == listDelimiter) {
            listDelimiter = DEFAULT_DELIMETER;
        }
        return listDelimiter;
    }

    /**
     * Setter accessor for attribute 'separator'.
     * @param separator
     * 		new value for 'separator '
     */
    public void setListDelimiter(String separator) {
        this.listDelimiter = separator;
    }

    /** {@inheritDoc} */
    public int size() {
        return (null == getValue()) ? 0 : getValue().size();
    }

    /** {@inheritDoc} */
    public boolean isEmpty() {
        return (null == getValue()) ? true : getValue().isEmpty();
    }

    /** {@inheritDoc} */
    public boolean contains(Object o) {
        return (null == getValue()) ? false : getValue().contains(o);
    }

    /** {@inheritDoc} */
    public Iterator<T> iterator() {
        return (null == getValue()) ? null : getValue().iterator();
    }

    /** {@inheritDoc} */
     public Object[] toArray() {
         return (null == getValue()) ? null : getValue().toArray();
    }

    /** {@inheritDoc} */
    public <X> X[] toArray(X[] a) {
        return (null == getValue()) ? null : getValue().toArray(a);
    }

    /** {@inheritDoc} */
    public boolean remove(Object o) {
        return (null == getValue()) ? false : getValue().remove(o);
    }

    /** {@inheritDoc} */
    public boolean containsAll(Collection<?> c) {
        return (null == getValue()) ? false : getValue().containsAll(c);
    }

    /** {@inheritDoc} */
    public boolean removeAll(Collection<?> c) {
        return (null == getValue()) ? false : getValue().removeAll(c);
    }

    /** {@inheritDoc} */
    public boolean retainAll(Collection<?> c) {
        return (null == getValue()) ? false : getValue().retainAll(c);
    }

    /** {@inheritDoc} */
    public void clear() {
        if (!isEmpty()) {
            getValue().clear();
        }
    }

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    public boolean addAll(Collection<? extends T> c) {
        if (value == null) {
            setValue((C) c);
        } else {
            return getValue().addAll(c);
        }
        return true;
    }

}
