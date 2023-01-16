package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

import java.math.BigDecimal;

/**
 * Serializer for {@link BigDecimal}.
 */
public class BigDecimalSerializer implements Serializer<BigDecimal> {

    @Override
    public BigDecimal deserialize(String expression) {
        if (expression == null) return null;
        try {
            return new BigDecimal(expression);
        } catch(NumberFormatException nbe) {
            throw new DeSerializationException(expression, BigDecimal.class, nbe);
        }
    }

    @Override
    public String serialize(BigDecimal object) {
        try {
            return (object == null) ? null : object.toPlainString();
        } catch(RuntimeException re) {
            throw new SerializationException(object);
        }
    }
}
