package org.ff4j.property.list;

import java.math.BigInteger;
import java.util.List;

import org.ff4j.property.PropertyBigInteger;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;


/**
 * Load property as list of {@link BigInteger}.
 */
public class PropertyListBigInteger extends PropertyList<BigInteger, PropertyBigInteger> {

    /**
     * Serializer.
     */
    private static final Serializer<List<BigInteger>> SERIALIZER = new GenericListSerializer<>(PropertyBigInteger.class);

    /**
     * {@inheritDoc}
     */
    public PropertyListBigInteger(String uid, BigInteger... value) {
        super(uid, value);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyListBigInteger(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Serializer<List<BigInteger>> getSerializer() {
        return SERIALIZER;
    }
}
