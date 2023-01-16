package org.ff4j.property;

import org.ff4j.property.serialize.DoubleSerializer;
import org.ff4j.property.serialize.Serializer;

/**
 * Represent Double
 */
public class PropertyDouble extends Property<Double> {

    /** String serializer. */
    private static final Serializer<Double> SERIALIZER = new DoubleSerializer();

    /** {@inheritDoc} */
    public PropertyDouble(String uid, String value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyDouble(String uid, Double value) { super(uid, value); }

    /** {@inheritDoc} */
    @Override
    public Serializer<Double> getSerializer() {
        return SERIALIZER;
    }

}
