package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

import java.math.BigInteger;

/**
 * Serializer for {@link BigInteger}.
 */
public class ClassSerializer<T> implements Serializer<Class<?>> {

    @Override
    public Class<?> deserialize(String expression) {
        if (expression == null) return null;
        try {
            return Class.forName(expression);
        } catch (ClassNotFoundException e) {
            throw new DeSerializationException(expression, BigInteger.class, e);
        } catch(RuntimeException nbe) {
            throw new DeSerializationException(expression, BigInteger.class, nbe);
        }
    }

    @Override
    public String serialize(Class<?> object) {
        try {
            return (object == null) ? null : object.getName();
        } catch(RuntimeException re) {
            throw new SerializationException(object);
        }
    }
}
