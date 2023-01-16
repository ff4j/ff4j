package org.ff4j.property;

import org.ff4j.property.serialize.DurationSerializer;
import org.ff4j.property.serialize.Serializer;

import java.time.Duration;

/**
 * Property to hold a duration.
 */
public class PropertyDuration extends Property< Duration > {

    /** String serializer. */
    private static final Serializer<Duration> SERIALIZER = new DurationSerializer();

    /** {@inheritDoc} */
    public PropertyDuration(String uid, String value) {
       super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyDuration(String uid, Duration value) {
       super(uid, value);
    }

    /** {@inheritDoc} */
    @Override
    public Serializer<Duration> getSerializer() { return SERIALIZER; }
}
