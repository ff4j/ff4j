package org.ff4j.property;

import org.ff4j.property.serialize.IntegerSerializer;
import org.ff4j.property.serialize.Serializer;

/**
 * Property to hold integer
 */
public class PropertyInteger extends Property<Integer> {

    /** String serializer. */
    private static final Serializer<Integer> SERIALIZER = new IntegerSerializer();

    /** {@inheritDoc} */
    public PropertyInteger(String uid, String value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyInteger(String uid, Integer value) { super(uid, value); }

    /** {@inheritDoc} */
    @Override
    public Serializer<Integer> getSerializer() {
        return SERIALIZER;
    }

}
