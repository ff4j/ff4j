package org.ff4j.property.serialize;

import org.ff4j.exception.DeSerializationException;
import org.ff4j.exception.SerializationException;

import java.util.Calendar;
import java.util.Date;

/**
 * Serializer for {@link Date}.
 *
 * The String representation is ISO: 20111203
 *
 * https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#BASIC_ISO_DATE
 */
public class CalendarSerializer implements Serializer<Calendar> {

    /** Leveraging the Local Date Serializer. */
    private static DateSerializer D_SERIALIZER = new DateSerializer();

    @Override
    public Calendar deserialize(String expression) {
        if (expression == null) return null;
        try {
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(D_SERIALIZER.deserialize(expression));
            return calendar;
        } catch(RuntimeException re) {
            throw new DeSerializationException(expression, Date.class, re);
        }
    }

    @Override
    public String serialize(Calendar object) {
        try {
            return D_SERIALIZER.serialize(object.getTime());
        } catch(RuntimeException re) {
            throw new SerializationException(object, re);
        }
    }
}
