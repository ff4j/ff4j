package org.ff4j.property.list;

import org.ff4j.property.PropertyLocalDateTime;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Load property as list of {@link Integer}.
 */
public class PropertyListLocalDateTime extends PropertyList<LocalDateTime, PropertyLocalDateTime> {

    /**
     * Serializer.
     */
    private static final Serializer<List<LocalDateTime>> SERIALIZER = new GenericListSerializer<>(PropertyLocalDateTime.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListLocalDateTime(String uid, LocalDateTime... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListLocalDateTime(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<LocalDateTime>> getSerializer() {
        return SERIALIZER;
    }
}
