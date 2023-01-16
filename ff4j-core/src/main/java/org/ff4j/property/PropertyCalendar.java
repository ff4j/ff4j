package org.ff4j.property;

import java.util.Calendar;

import org.ff4j.property.serialize.CalendarSerializer;
import org.ff4j.property.serialize.Serializer;

/**
 * Property of type {@link Calendar}.
 */
public class PropertyCalendar extends Property<Calendar> {

    /** String serializer. */
    private static final Serializer<Calendar> SERIALIZER = new CalendarSerializer();

    /** {@inheritDoc} */
    public PropertyCalendar(String uid, Calendar value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyCalendar(String uid, String value) { super(uid, value); }

    /** {@inheritDoc} */
    @Override
    public Serializer<Calendar> getSerializer() {
        return SERIALIZER;
    }

}
