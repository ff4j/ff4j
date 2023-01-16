package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

import java.math.BigInteger;

/**
 * Serializer for {@link BigInteger}.
 */
public class BigIntegerSerializer implements Serializer<BigInteger> {

    @Override
    public BigInteger deserialize(String expression) {
        if (expression == null) return null;
        try {
            return new BigInteger(expression);
        } catch(NumberFormatException nbe) {
            throw new DeSerializationException(expression, BigInteger.class, nbe);
        }
    }

    @Override
    public String serialize(BigInteger object) {
        try {
            return (object == null) ? null : object.toString();
        } catch(RuntimeException re) {
            throw new SerializationException(object);
        }
    }
}
