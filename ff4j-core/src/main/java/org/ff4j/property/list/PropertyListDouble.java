package org.ff4j.property.list;

import org.ff4j.property.PropertyDouble;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.Calendar;
import java.util.List;

/**
 * Load property as list of {@link Calendar}.
 */
public class PropertyListDouble extends PropertyList<Double, PropertyDouble> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Double>> SERIALIZER = new GenericListSerializer<>(PropertyDouble.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListDouble(String uid, Double... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListDouble(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Double>> getSerializer() {
        return SERIALIZER;
    }
}
