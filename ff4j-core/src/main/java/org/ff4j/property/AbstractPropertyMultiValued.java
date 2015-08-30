package org.ff4j.property;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;

/**
 * Super class to work with multivalued properties.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 *
 * @param <T>
 *      current inner type
 * @param <C>
 *      current collection type
 */
public abstract class AbstractPropertyMultiValued < T, C extends Collection< T>> extends AbstractProperty < C > implements Collection< T > {

    /** Serial. */
    private static final long serialVersionUID = 1L;
    
    /** required if should be splip. */
    public String separator = ",";
    
    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    @Override
    public C fromString(String v) {
        if (v == null) return null;
        // Caution, separator should not be escaped charaters like ., ? 
        String[] items = v.split(separator);
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
     * Getter accessor for attribute 'separator'.
     *
     * @return
     *       current value of 'separator'
     */
    public String getSeparator() {
        return separator;
    }

    /**
     * Setter accessor for attribute 'separator'.
     * @param separator
     * 		new value for 'separator '
     */
    public void setSeparator(String separator) {
        this.separator = separator;
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
