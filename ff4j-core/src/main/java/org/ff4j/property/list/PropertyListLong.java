package org.ff4j.property.list;

import org.ff4j.property.PropertyLong;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.List;

/**
 * Load property as list of {@link Integer}.
 */
public class PropertyListLong extends PropertyList<Long, PropertyLong> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Long>> SERIALIZER = new GenericListSerializer<>(PropertyLong.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListLong(String uid, Long... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListLong(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Long>> getSerializer() {
        return SERIALIZER;
    }
}
