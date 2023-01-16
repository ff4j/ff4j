package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

/**
 * Serializer for {@link Integer}.
 */
public class IntegerSerializer implements Serializer<Integer> {

    @Override
    public Integer deserialize(String expression) {
        if (expression == null) return null;
        try {
            return Integer.valueOf(expression);
        } catch(NumberFormatException nbe) {
            throw new DeSerializationException(expression, Byte.class, nbe);
        }
    }

    @Override
    public String serialize(Integer object) {
        try {
            return (object == null) ? null :  String.valueOf(object);
        } catch(RuntimeException re) {
            throw new SerializationException(object);
        }
    }
}
