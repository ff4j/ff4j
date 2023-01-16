package org.ff4j.property;

import org.ff4j.property.serialize.InstantSerializer;
import org.ff4j.property.serialize.Serializer;

import java.time.Instant;

/**
 * Property Instant.
 */
public class PropertyInstant extends Property<Instant> {

    /**
     * String serializer.
     */
    private static final Serializer<Instant> SERIALIZER = new InstantSerializer();

    /**
     * {@inheritDoc}
     */
    public PropertyInstant(String uid, String value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyInstant(String uid, Instant value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<Instant> getSerializer() {
        return SERIALIZER;
    }

}
