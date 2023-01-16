package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

/**
 * Serializer for {@link Long}.
 */
public class LongSerializer implements Serializer<Long> {

    @Override
    public Long deserialize(String expression) {
        if (expression == null) return null;
        try {
            return Long.valueOf(expression);
        } catch(NumberFormatException nbe) {
            throw new DeSerializationException(expression, Byte.class, nbe);
        }
    }

    @Override
    public String serialize(Long object) {
        try {
            return (object == null) ? null :  String.valueOf(object);
        } catch(RuntimeException re) {
            throw new SerializationException(object);
        }
    }
}
