package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

/**
 * Serializer for {@link Byte}.
 */
public class DoubleSerializer implements Serializer<Double> {

    @Override
    public Double deserialize(String expression) {
        if (expression == null) return null;
        try {
            return Double.valueOf(expression);
        } catch(NumberFormatException nbe) {
            throw new DeSerializationException(expression, Byte.class, nbe);
        }
    }

    @Override
    public String serialize(Double object) {
        try {
            return (object == null) ? null :  String.valueOf(object);
        } catch(RuntimeException re) {
            throw new SerializationException(object);
        }
    }
}
