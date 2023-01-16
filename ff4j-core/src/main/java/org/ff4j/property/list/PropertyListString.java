package org.ff4j.property.list;

import org.ff4j.property.PropertyString;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.List;

/**
 * Load property as list of {@link String}.
 */
public class PropertyListString extends PropertyList<String, PropertyString> {

    /**
     * Serializer.
     */
    private static final Serializer<List<String>> SERIALIZER = new GenericListSerializer<>(PropertyString.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListString(String uid, String... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListString(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<String>> getSerializer() {
        return SERIALIZER;
    }
}
