package org.ff4j.property.list;

import org.ff4j.property.PropertyInstant;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.time.Instant;
import java.util.List;

/**
 * Load property as list of {@link Boolean}.
 */
public class PropertyListInstant extends PropertyList<Instant, PropertyInstant> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Instant>> SERIALIZER = new GenericListSerializer<>(PropertyInstant.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListInstant(String uid, Instant... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListInstant(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Instant>> getSerializer() {
        return SERIALIZER;
    }
}
