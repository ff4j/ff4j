package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

import java.time.Duration;

/**
 * Serializer for the Duration type.
 */
public class DurationSerializer implements Serializer<Duration> {

    @Override
    public Duration deserialize(String expression) {
        if (expression == null) return null;
        try {
            return Duration.parse(expression);
        } catch(RuntimeException nbe) {
            throw new DeSerializationException(expression, Byte.class, nbe);
        }
    }

    @Override
    public String serialize(Duration object) {
        try {
            return (object == null) ? null : object.toString();
        } catch(RuntimeException re) {
            throw new SerializationException(object);
        }
    }
}
