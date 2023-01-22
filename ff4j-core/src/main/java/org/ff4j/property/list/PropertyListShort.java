package org.ff4j.property.list;

import org.ff4j.property.PropertyShort;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.List;

/**
 * Load property as list of {@link Integer}.
 */
public class PropertyListShort extends PropertyList<Short, PropertyShort> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Short>> SERIALIZER = new GenericListSerializer<>(PropertyShort.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListShort(String uid, Short... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListShort(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Short>> getSerializer() {
        return SERIALIZER;
    }
}
