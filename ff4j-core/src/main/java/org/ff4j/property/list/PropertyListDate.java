package org.ff4j.property.list;

import org.ff4j.property.PropertyDate;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * Load property as list of {@link Calendar}.
 */
public class PropertyListDate extends PropertyList<Date, PropertyDate> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Date>> SERIALIZER = new GenericListSerializer<>(PropertyDate.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListDate(String uid, Date... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListDate(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Date>> getSerializer() {
        return SERIALIZER;
    }
}
