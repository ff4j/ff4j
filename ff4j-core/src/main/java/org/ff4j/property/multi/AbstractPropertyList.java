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


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;

/**
 * SuperClass for property as lists.
 *
 * @author Cedrick Lunven (@clunven)
 *
 * @param <T>
 *      current type
 */
public abstract class AbstractPropertyList < T > extends AbstractPropertyMultiValued< T, List <T>> implements List<T> {

    /** Serial. */
    private static final long serialVersionUID = 4064427839404299895L;
    
    /**
     * Default constructor.
     */
    public AbstractPropertyList() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public AbstractPropertyList(String name) {
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
    public AbstractPropertyList(String uid, String value) {
       super(uid, value);
    }
    
    /**
     * Constructor by T expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    public AbstractPropertyList(String uid, List<T> value) {
       super(uid, value);
    }
    
    /**
     * Constructor by T expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    @SafeVarargs
    public AbstractPropertyList(String uid, T... value) {
       super(uid, Arrays.asList(value));
    }
    
    /** {@inheritDoc} */
    @Override
    public List<T> fromString(String v) {
        List<T> list = super.fromString(v);
        if (list == null) return null;
        return new ArrayList<T>(list);
    }

    /** {@inheritDoc} */
    public boolean addAll(int index, Collection<? extends T> c) {
        return (null == getValue()) ? false : getValue().addAll(index, c);
    }

    /** {@inheritDoc} */
    public T get(int index) {
        return (null == getValue()) ? null : getValue().get(index);
    }

    /** {@inheritDoc} */
    public T set(int index, T element) {
        return (null == getValue()) ? null : getValue().set(index, element);
    }

    /** {@inheritDoc} */
    public void add(int index, T element) {
        if (null == getValue()) {
            value = new ArrayList<T>();
        }
        getValue().add(index, element);
    }

    /** {@inheritDoc} */
    public T remove(int index) {
        return (null == getValue()) ? null : getValue().remove(index);
    }

    /** {@inheritDoc} */
    public int indexOf(Object o) {
        return (null == getValue()) ? -1 : getValue().indexOf(o);
    }

    /** {@inheritDoc} */
    public int lastIndexOf(Object o) {
        return (null == getValue()) ? -1 : getValue().lastIndexOf(o);
    }

    /** {@inheritDoc} */
    public ListIterator<T> listIterator() {
        return (null == getValue()) ? null : getValue().listIterator();
    }

    /** {@inheritDoc} */
    public ListIterator<T> listIterator(int index) {
        return (null == getValue()) ? null : getValue().listIterator(index);
    }

    /** {@inheritDoc} */
    public List<T> subList(int fromIndex, int toIndex) {
        return (null == getValue()) ? null : getValue().subList(fromIndex, toIndex);
    }

}
