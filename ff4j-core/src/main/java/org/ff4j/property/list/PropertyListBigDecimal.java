package org.ff4j.property.list;

import java.math.BigDecimal;
import java.util.List;

import org.ff4j.property.PropertyBigDecimal;
import org.ff4j.property.serialize.GenericListSerializer;
import org.ff4j.property.serialize.Serializer;

/**
 * Load property as list of {@link BigDecimal}.
 */
public class PropertyListBigDecimal extends PropertyList<BigDecimal, PropertyBigDecimal> {

    /** Serializer. */
    private static final Serializer<List<BigDecimal>> SERIALIZER = new GenericListSerializer<>(PropertyBigDecimal.class);

    /** {@inheritDoc} */
    public PropertyListBigDecimal(String uid, BigDecimal... value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyListBigDecimal(String uid, String valueAsString) {
        super(uid, valueAsString);
    }

    /** {@inheritDoc} */
    @Override
    public Serializer<List<BigDecimal>> getSerializer() {
        return SERIALIZER;
    }

}
