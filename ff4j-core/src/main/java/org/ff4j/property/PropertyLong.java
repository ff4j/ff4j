package org.ff4j.property;

import org.ff4j.property.serialize.LongSerializer;
import org.ff4j.property.serialize.Serializer;

/**
 * Property of type long.
 */
public class PropertyLong extends Property<Long> {

    /** String serializer. */
    private static final Serializer<Long> SERIALIZER = new LongSerializer();

    /** {@inheritDoc} */
    public PropertyLong(String uid, String value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyLong(String uid, Long value) { super(uid, value); }

    /** {@inheritDoc} */
    @Override
    public Serializer<Long> getSerializer() {
        return SERIALIZER;
    }

}