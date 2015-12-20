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


import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.ff4j.property.AbstractProperty;

/**
 * 
 * @author Cedrick Lunven (@clunven)</a>
 *
 * @param <T>
 * @param <M>
 */
public class AbstractPropertyMap < T, M extends Map<String, ? extends T>> extends AbstractProperty < M > implements Map< String, T > {

    /** serial. */
    private static final long serialVersionUID = 2612494170643655559L;

    /** {@inheritDoc} */
    public int size() {
        return (null == getValue()) ? -1 : getValue().size();
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    /** {@inheritDoc} */
    public boolean containsKey(Object key) {
        return value.containsKey(key);
    }

    @Override
    /** {@inheritDoc} */
    public boolean containsValue(Object obj) {
        return value.containsValue(obj);
    }

    @Override
    public T get(Object key) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public T put(String key, T value) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public T remove(Object key) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void putAll(Map<? extends String, ? extends T> m) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Set<String> keySet() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Collection<T> values() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Set<java.util.Map.Entry<String, T>> entrySet() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public M fromString(String v) {
        // TODO Auto-generated method stub
        return null;
    }

}
