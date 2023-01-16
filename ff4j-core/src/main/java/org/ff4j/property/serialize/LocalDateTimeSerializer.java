package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Serializer for {@link LocalDateTime}.
 */
public class LocalDateTimeSerializer implements Serializer<LocalDateTime> {

    @Override
    public LocalDateTime deserialize(String expression) {
        if (expression == null) return null;
        try {
            // Get a local date with java.time
            return LocalDateTime.parse(expression, DateTimeFormatter.ISO_DATE_TIME);
        } catch(RuntimeException re) {
            throw new DeSerializationException(expression, LocalDate.class, re);
        }
    }

    @Override
    public String serialize(LocalDateTime parsedDate) {
        try {
            // Format as String
            return parsedDate.format(DateTimeFormatter.ISO_DATE_TIME);
        } catch(RuntimeException re) {
            throw new SerializationException(parsedDate, re);
        }
    }
}
