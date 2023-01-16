package org.ff4j.property;

import org.ff4j.property.serialize.Serializer;
import org.ff4j.property.serialize.BigDecimalSerializer;

import java.math.BigDecimal;

/**
 * Implementation of a property of type {@link BigDecimal}.
 */
public class PropertyBigDecimal extends Property< BigDecimal > {

    /** String serializer. */
    private static final Serializer<BigDecimal> BIG_DECIMAL_TYPE_SERIALIZER = new BigDecimalSerializer();

    /** {@inheritDoc} */
    public PropertyBigDecimal(String uid, String value) {
       super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyBigDecimal(String uid, BigDecimal value) {
       super(uid, value);
    }

    /** {@inheritDoc} */
    @Override
    public Serializer<BigDecimal> getSerializer() {
        return BIG_DECIMAL_TYPE_SERIALIZER;
    }

}
