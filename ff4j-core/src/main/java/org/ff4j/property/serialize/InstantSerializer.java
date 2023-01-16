package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

import java.time.*;
import java.time.format.DateTimeFormatter;

/**
 * Serializer for {@link Instant}.
 */
public class InstantSerializer implements Serializer<Instant> {

    @Override
    public Instant deserialize(String expression) {
        if (expression == null) return null;
        try {
            // Get a local date with java.time
            return LocalDateTime.parse(expression, DateTimeFormatter.ISO_INSTANT).toInstant(ZoneOffset.UTC);
        } catch(RuntimeException re) {
            throw new DeSerializationException(expression, LocalDate.class, re);
        }
    }

    @Override
    public String serialize(Instant parsedDate) {
        try {
            // Format as String
            return LocalDateTime.ofInstant(parsedDate, ZoneOffset.UTC).format(DateTimeFormatter.ISO_INSTANT);
        } catch(RuntimeException re) {
            throw new SerializationException(parsedDate, re);
        }
    }
}
