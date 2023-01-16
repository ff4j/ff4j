package org.ff4j.property.list;

import org.ff4j.property.PropertyCalendar;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.Calendar;
import java.util.List;

/**
 * Load property as list of {@link Calendar}.
 */
public class PropertyListCalendar extends PropertyList<Calendar, PropertyCalendar> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Calendar>> SERIALIZER = new GenericListSerializer<>(PropertyCalendar.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListCalendar(String uid, Calendar... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListCalendar(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Calendar>> getSerializer() {
        return SERIALIZER;
    }
}
