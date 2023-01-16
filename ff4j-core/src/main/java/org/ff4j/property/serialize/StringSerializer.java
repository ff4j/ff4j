package org.ff4j.property.serialize;

/**
 * Serializer for {@link String}.
 */
public class StringSerializer implements Serializer<String> {

    @Override
    public String deserialize(String expression) {
       return expression;
    }

    @Override
    public String serialize(String object) {
        return object;
    }
}
