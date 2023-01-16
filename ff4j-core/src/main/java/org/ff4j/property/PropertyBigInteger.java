package org.ff4j.property;

import org.ff4j.property.serialize.Serializer;
import org.ff4j.property.serialize.BigIntegerSerializer;

import java.math.BigInteger;

/**
 * Implementation of a property of type {@link BigInteger}.
 */
public class PropertyBigInteger extends Property< BigInteger > {

    /** String serializer. */
    private static final Serializer<BigInteger> BIG_INTEGER_TYPE_SERIALIZER = new BigIntegerSerializer();

    /** {@inheritDoc} */
    public PropertyBigInteger(String uid, String value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyBigInteger(String uid, BigInteger value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    @Override
    public Serializer<BigInteger> getSerializer() {
        return BIG_INTEGER_TYPE_SERIALIZER;
    }

}
