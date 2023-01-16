package org.ff4j.property.list;

import org.ff4j.property.PropertyByte;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.List;

/**
 * Load property as list of {@link Byte}.
 */
public class PropertyListByte extends PropertyList<Byte, PropertyByte> {

    /**
     * Serializer.
     */
    private static final Serializer<List<Byte>> SERIALIZER = new GenericListSerializer<>(PropertyByte.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListByte(String uid, Byte... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListByte(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<Byte>> getSerializer() {
        return SERIALIZER;
    }
}
