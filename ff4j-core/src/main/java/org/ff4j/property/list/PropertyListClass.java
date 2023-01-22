package org.ff4j.property.list;

import org.ff4j.property.PropertyClass;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.List;

/**
 * Load property as list of {@link Boolean}.
 */
public class PropertyListClass extends PropertyList<Class<?>, PropertyClass> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Class<?>>> SERIALIZER = new GenericListSerializer<>(PropertyClass.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListClass(String uid, Class<?>... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListClass(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Class<?>>> getSerializer() {
        return SERIALIZER;
    }
}
