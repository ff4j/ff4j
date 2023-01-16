package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

/**
 * Serializer for {@link Byte}.
 */
public class ByteSerializer implements Serializer<Byte> {

    @Override
    public Byte deserialize(String expression) {
        if (expression == null) return null;
        try {
            return Byte.valueOf(expression);
        } catch(RuntimeException nbe) {
            throw new DeSerializationException(expression, Byte.class, nbe);
        }
    }

    @Override
    public String serialize(Byte object) {
        try {
            return (object == null) ? null : object.toString();
        } catch(RuntimeException re) {
            throw new SerializationException(object);
        }
    }
}
