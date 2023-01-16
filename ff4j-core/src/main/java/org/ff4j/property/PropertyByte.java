package org.ff4j.property;


import org.ff4j.property.serialize.ByteSerializer;
import org.ff4j.property.serialize.Serializer;

/**
 * Byte Property.
 */
public class PropertyByte extends Property< Byte > {

    /** String serializer. */
    private static final Serializer<Byte> SERIALIZER = new ByteSerializer();

    /** {@inheritDoc} */
    public PropertyByte(String uid, Byte value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyByte(String uid, String value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    @Override
    public Serializer<Byte> getSerializer() {
        return SERIALIZER;
    }


}
