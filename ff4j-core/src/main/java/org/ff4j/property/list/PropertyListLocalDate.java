package org.ff4j.property.list;

import org.ff4j.property.PropertyLocalDate;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.List;

/**
 * Load property as list of {@link Calendar}.
 */
public class PropertyListLocalDate extends PropertyList<LocalDate, PropertyLocalDate> {

    /**
     * Serializer.
     */
    private static final Serializer<List<LocalDate>> SERIALIZER = new GenericListSerializer<>(PropertyLocalDate.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListLocalDate(String uid, LocalDate... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListLocalDate(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<LocalDate>> getSerializer() {
        return SERIALIZER;
    }
}
