package org.ff4j.property;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Enable Multivalued properties. 
 *
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <T>
 *      basic type (int, short....)
 * @param <K>
 * `    simple property type using associated to same basic ex. String and PropertyString.
 */
public abstract class PropertyList<T, K extends Property<T>> extends Property < List < T> > {

    /** Serialisation. */
    private static final long serialVersionUID = 7171347992502786427L;
    
    /** Instance of simple property to use `fromString()` on each element of the list. */
    private K property;
    
    /** Use the delimiter of lists. */
    public static String listDelimiter = ",";

    /**
     * Constructors leveraging Property<List<X>> and initializing Property<X>
     *
     * @param uid
     *      property ID
     */
    public PropertyList(String uid) {
        super(uid);
        initProperty(uid);
    }
    public PropertyList(String uid, String valueAsString) {
        super(uid, valueAsString);
        initProperty(uid);
    }
    public PropertyList(String uid, List<T> value) {
        super(uid, value);
        initProperty(uid);
    }
    @SuppressWarnings("unchecked")
    public PropertyList(String uid, T... value) {
        super(uid, Arrays.asList(value));
        initProperty(uid);
    }
    
    /** {@inheritDoc} */
    @Override
    public List<T> fromString(String v) {
        if (v == null) return null;
        if (property == null) initProperty(uid);
        return Arrays.stream(v.split(listDelimiter))
                     .map(String::trim)
                     .map(property::fromString)
                     .collect(Collectors.toList());
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
        // Handle Collections
        if (Collection.class.isAssignableFrom(get().getClass())) {
           Collection<T> collection = (Collection<T>) get();
           if (collection == null || collection.isEmpty()) {
               return "";
           }
           Iterator<T> it = collection.iterator();
           StringBuilder sb = new StringBuilder(it.next().toString());
           while (it.hasNext()) {
               sb.append(listDelimiter);
               sb.append(it.next());
           }
           return sb.toString();
        }
        return get().toString();
    }
    
    @SuppressWarnings("unchecked")
    protected void initProperty(String uid) {
        try {
            Class<K> persistentClass = (Class<K>)
                    ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[1];
            property = persistentClass.getConstructor(String.class).newInstance(uid);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException
                | NoSuchMethodException | SecurityException e) {
            throw new IllegalArgumentException("Cannot use reflection to instanciate Generic type :"
                    + "constructor with single argument string should exist", e);
        }
    }
    
}
