package org.ff4j.property;

import java.time.LocalDate;

import org.ff4j.property.serialize.LocalDateSerializer;
import org.ff4j.property.serialize.Serializer;

/**
 * Hold LocalDateTime property.
 */
public class PropertyLocalDate extends Property<LocalDate> {

    /**
     * String serializer.
     */
    private static final Serializer<LocalDate> SERIALIZER = new LocalDateSerializer();

    /**
     * {@inheritDoc}
     */
    public PropertyLocalDate(String uid, LocalDate value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyLocalDate(String uid, String value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<LocalDate> getSerializer() {
        return SERIALIZER;
    }
}

