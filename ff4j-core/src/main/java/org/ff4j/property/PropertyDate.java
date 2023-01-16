package org.ff4j.property;


import org.ff4j.property.serialize.DateSerializer;
import org.ff4j.property.serialize.Serializer;

import java.util.Date;

/**
 * Implementation of Property to work with Date.
 */
public class PropertyDate extends Property< Date > {

    /** String serializer. */
    private static final Serializer<Date> SERIALIZER = new DateSerializer();

    /** {@inheritDoc} */
    public PropertyDate(String uid, Date value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyDate(String uid, String value) { super(uid, value); }

    /** {@inheritDoc} */
    @Override
    public Serializer<Date> getSerializer() {
        return SERIALIZER;
    }
}
