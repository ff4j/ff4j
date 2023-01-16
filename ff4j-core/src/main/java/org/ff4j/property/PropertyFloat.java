package org.ff4j.property;

import org.ff4j.property.serialize.FloatSerializer;
import org.ff4j.property.serialize.Serializer;

/**
 * Property with Float
 */
public class PropertyFloat  extends Property<Float> {

    /** String serializer. */
    private static final Serializer<Float> SERIALIZER = new FloatSerializer();

    /** {@inheritDoc} */
    public PropertyFloat(String uid, String value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyFloat(String uid, Float value) { super(uid, value); }

    /** {@inheritDoc} */
    @Override
    public Serializer<Float> getSerializer() {
        return SERIALIZER;
    }

}
