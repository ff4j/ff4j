package org.ff4j.property;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public class AbstractPropertyMap < T, M extends Map<String, ? extends T>> extends AbstractProperty < M > implements Map< String, T > {

    /** serial. */
    private static final long serialVersionUID = 2612494170643655559L;

    @Override
    public int size() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public boolean isEmpty() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean containsKey(Object key) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean containsValue(Object value) {
        // TODO Auto-generated method stub
        return false;
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
