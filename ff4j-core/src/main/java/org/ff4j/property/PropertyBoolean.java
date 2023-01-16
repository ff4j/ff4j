package org.ff4j.property;

import org.ff4j.property.serialize.Serializer;
import org.ff4j.property.serialize.BooleanSerializer;

/**
 * Boolean Property.
 */
public class PropertyBoolean extends Property< Boolean > {

    /** String serializer. */
    private static final Serializer<Boolean> BOOLEAN_TYPE_SERIALIZER = new BooleanSerializer();

    /** {@inheritDoc} */
    public PropertyBoolean(String uid, boolean value) {
        this(uid, String.valueOf(value));
    }

    /** {@inheritDoc} */
    public PropertyBoolean(String uid, String value) {
       super(uid, value);
       setFixedValues(Boolean.TRUE, Boolean.FALSE);
    }

    /** {@inheritDoc} */
    @Override
    public Serializer<Boolean> getSerializer() {
        return BOOLEAN_TYPE_SERIALIZER;
    }

}
