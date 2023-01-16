package org.ff4j.property;

import org.ff4j.property.serialize.Serializer;
import org.ff4j.property.serialize.ShortSerializer;

/**
 * Hold property of type {@link Short}
 */
public class PropertyShort extends Property<Short> {

    /** String serializer. */
    private static final Serializer<Short> SERIALIZER = new ShortSerializer();

    /** {@inheritDoc} */
    public PropertyShort(String uid, String value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyShort(String uid, Short value) { super(uid, value); }

    /** {@inheritDoc} */
    @Override
    public Serializer<Short> getSerializer() {
        return SERIALIZER;
    }

}
