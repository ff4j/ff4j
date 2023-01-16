package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * Serializer for {@link LocalDate}.
 */
public class LocalDateSerializer implements Serializer<LocalDate> {

    @Override
    public LocalDate deserialize(String expression) {
        if (expression == null) return null;
        try {
            // Get a local date with java.time
            return LocalDate.parse(expression, DateTimeFormatter.ISO_DATE);
        } catch(RuntimeException re) {
            throw new DeSerializationException(expression, LocalDate.class, re);
        }
    }

    @Override
    public String serialize(LocalDate parsedDate) {
        try {
            // Format as String
            return parsedDate.format(DateTimeFormatter.ISO_DATE);
        } catch(RuntimeException re) {
            throw new SerializationException(parsedDate, re);
        }
    }
}
