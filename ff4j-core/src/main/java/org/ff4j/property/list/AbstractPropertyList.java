package org.ff4j.property.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;

import org.ff4j.property.AbstractPropertyMultiValued;

/**
 * SuperClass for property as lists.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 *
 * @param <T>
 *      current type
 */
public class AbstractPropertyList < T > extends AbstractPropertyMultiValued< T, List <T>> implements List<T>{

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
    

    /** {@inheritDoc} */
    @Override
    public List<T> fromString(String v) {
        return new ArrayList<T>(super.fromString(v));
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
