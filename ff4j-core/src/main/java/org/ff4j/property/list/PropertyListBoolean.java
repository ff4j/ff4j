package org.ff4j.property.list;

import org.ff4j.property.PropertyBoolean;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.List;

/**
 * Load property as list of {@link Boolean}.
 */
public class PropertyListBoolean extends PropertyList<Boolean, PropertyBoolean> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Boolean>> SERIALIZER = new GenericListSerializer<>(PropertyBoolean.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListBoolean(String uid, Boolean... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListBoolean(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Boolean>> getSerializer() {
        return SERIALIZER;
    }
}
