package org.ff4j.property.list;

import org.ff4j.property.PropertyFloat;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.Calendar;
import java.util.List;

/**
 * Load property as list of {@link Calendar}.
 */
public class PropertyListFloat extends PropertyList<Float, PropertyFloat> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Float>> SERIALIZER = new GenericListSerializer<>(PropertyFloat.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListFloat(String uid, Float... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListFloat(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Float>> getSerializer() {
        return SERIALIZER;
    }
}
