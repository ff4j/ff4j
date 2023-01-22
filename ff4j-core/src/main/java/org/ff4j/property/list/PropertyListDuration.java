package org.ff4j.property.list;

import org.ff4j.property.PropertyDuration;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.time.Duration;
import java.util.Calendar;
import java.util.List;

/**
 * Load property as list of {@link Calendar}.
 */
public class PropertyListDuration extends PropertyList<Duration, PropertyDuration> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Duration>> SERIALIZER = new GenericListSerializer<>(PropertyDuration.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListDuration(String uid, Duration... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListDuration(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Duration>> getSerializer() {
        return SERIALIZER;
    }
}
