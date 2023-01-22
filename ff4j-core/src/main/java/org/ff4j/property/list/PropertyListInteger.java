package org.ff4j.property.list;

import org.ff4j.property.PropertyInteger;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.List;

/**
 * Load property as list of {@link Integer}.
 */
public class PropertyListInteger extends PropertyList<Integer, PropertyInteger> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Integer>> SERIALIZER = new GenericListSerializer<>(PropertyInteger.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListInteger(String uid, Integer... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListInteger(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Integer>> getSerializer() {
        return SERIALIZER;
    }
}
