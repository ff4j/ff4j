package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

/**
 * Serializer for {@link Date}.
 *
 * The String representation is ISO: 20111203
 *
 * https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#BASIC_ISO_DATE
 */
public class DateSerializer implements Serializer<Date> {

    /** Leveraging the Local Date Serializer. */
    private static LocalDateSerializer LD_SERIALIZER = new LocalDateSerializer();

    @Override
    public Date deserialize(String expression) {
        if (expression == null) return null;
        try {
            // Get a local date with java.time
            LocalDate parsedDate = LD_SERIALIZER.deserialize(expression);
            // Back to Java util Date
            return Date.from(parsedDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
        } catch(RuntimeException re) {
            throw new DeSerializationException(expression, Date.class, re);
        }
    }

    @Override
    public String serialize(Date object) {
        try {
            // Parse as local date
            LocalDate parsedDate = Instant.ofEpochMilli(object.getTime()).atZone(ZoneId.systemDefault()).toLocalDate();
            // Format as String
            return LD_SERIALIZER.serialize(parsedDate);
        } catch(RuntimeException re) {
            throw new SerializationException(object, re);
        }
    }
}
