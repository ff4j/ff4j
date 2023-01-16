package org.ff4j.property;

import java.time.LocalDateTime;

import org.ff4j.property.serialize.LocalDateTimeSerializer;
import org.ff4j.property.serialize.Serializer;

/**
 * Hold LocalDateTime property.
 */
public class PropertyLocalDateTime extends Property<LocalDateTime> {

    /**
     * String serializer.
     */
    private static final Serializer<LocalDateTime> SERIALIZER = new LocalDateTimeSerializer();

    /**
     * {@inheritDoc}
     */
    public PropertyLocalDateTime(String uid, LocalDateTime value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyLocalDateTime(String uid, String value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<LocalDateTime> getSerializer() {
        return SERIALIZER;
    }
}

