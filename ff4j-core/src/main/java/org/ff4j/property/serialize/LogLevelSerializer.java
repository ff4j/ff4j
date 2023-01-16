package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;
import org.ff4j.property.PropertyLogLevel;

/**
 * Serializer for {@link Integer}.
 */
public class LogLevelSerializer implements Serializer<PropertyLogLevel.LogLevel> {

    @Override
    public PropertyLogLevel.LogLevel deserialize(String expression) {
        if (expression == null) return null;
        try {
            return PropertyLogLevel.LogLevel.valueOf(expression);
        } catch(RuntimeException nbe) {
            throw new DeSerializationException(expression, Byte.class, nbe);
        }
    }

    @Override
    public String serialize(PropertyLogLevel.LogLevel object) {
        try {
            return (object == null) ? null : String.valueOf(object);
        } catch(RuntimeException re) {
            throw new SerializationException(object);
        }
    }
}
