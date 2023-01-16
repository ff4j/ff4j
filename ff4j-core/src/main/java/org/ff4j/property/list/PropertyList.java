package org.ff4j.property.list;

import java.lang.reflect.ParameterizedType;
import java.util.Arrays;
import java.util.List;

import org.ff4j.property.Property;

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
public abstract class PropertyList<T, K extends Property<T>> extends Property < List < T > > {
    
	/** local reference to property. */
	public K property;
	
    /**
     * Constructors leveraging Property<List<X>> and initializing Property<X>
     *
     * @param uid
     *      property ID
     */
    public PropertyList(String uid, String valueAsString) {
        super(uid, valueAsString);
        initProperty(uid);
    }
    
    /**
     * Constructor with array of values.
     * 
     * @param uid
     * 		property identifier
     * @param value
     * 		property value
     */
    @SuppressWarnings("unchecked")
    public PropertyList(String uid, T... value) {
        super(uid, (String) null);
        if (value != null) this.value = Arrays.asList(value);
        initProperty(uid);
    }

    /**
     * Initialization with identifier.
     * @param uid
     *      identifier
     */
    @SuppressWarnings("unchecked")
    protected void initProperty(String uid) {
        try {
            Class<K> persistentClass = (Class<K>)
                    ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[1];
            property = persistentClass.getConstructor(String.class).newInstance(uid);
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot initialize type :"
                    + "constructor with argument String does not exist", e);
        }
    }
    
}
