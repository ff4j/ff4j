package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

/**
 * Serializer for {@link Byte}.
 */
public class FloatSerializer implements Serializer<Float> {

    @Override
    public Float deserialize(String expression) {
        if (expression == null) return null;
        try {
            return Float.valueOf(expression);
        } catch(NumberFormatException nbe) {
            throw new DeSerializationException(expression, Byte.class, nbe);
        }
    }

    @Override
    public String serialize(Float object) {
        try {
            return (object == null) ? null :  String.valueOf(object);
        } catch(RuntimeException re) {
            throw new SerializationException(object);
        }
    }
}
