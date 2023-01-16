package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

/**
 * Serializer for {@link Short}.
 */
public class ShortSerializer implements Serializer<Short> {

    @Override
    public Short deserialize(String expression) {
        if (expression == null) return null;
        try {
            return Short.valueOf(expression);
        } catch(NumberFormatException nbe) {
            throw new DeSerializationException(expression, Byte.class, nbe);
        }
    }

    @Override
    public String serialize(Short object) {
        try {
            return (object == null) ? null :  String.valueOf(object);
        } catch(RuntimeException re) {
            throw new SerializationException(object);
        }
    }
}
