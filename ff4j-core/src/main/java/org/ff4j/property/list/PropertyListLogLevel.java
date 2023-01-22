package org.ff4j.property.list;

import org.ff4j.property.PropertyInteger;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.List;

/**
 * Load property as list of {@link Integer}.
 */
public class PropertyListLogLevel extends PropertyList<PropertyLogLevel.LogLevel, PropertyLogLevel> {

    /**
     * Serializer.
     */
    private static final Serializer<List<PropertyLogLevel.LogLevel>> SERIALIZER =
            new GenericListSerializer<>(PropertyLogLevel.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListLogLevel(String uid, PropertyLogLevel.LogLevel... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListLogLevel(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<PropertyLogLevel.LogLevel>> getSerializer() {
        return SERIALIZER;
    }
}
