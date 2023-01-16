package org.ff4j.property.serialize;

import java.math.BigInteger;

/**
 * Serializer for {@link BigInteger}.
 */
public class BooleanSerializer implements Serializer<Boolean> {

    @Override
    public Boolean deserialize(String expression) {
        return (expression == null) ? null : Boolean.parseBoolean(expression);
    }

    @Override
    public String serialize(Boolean object) {
        return (object == null) ? null : object.toString();
    }
}
