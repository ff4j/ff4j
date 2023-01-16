package org.ff4j.property;

import org.ff4j.property.serialize.ClassSerializer;
import org.ff4j.property.serialize.Serializer;

/**
 * Implementation of Property for java {@link Class}.
 */
public class PropertyClass extends Property<Class<?>> {

    /** String serializer. */
    private static final Serializer<Class<?>> SERIALIZER = new ClassSerializer<Object>();

    /** {@inheritDoc} */
    public PropertyClass(String uid, Class<?> value) {
        super(uid, value);
    }

    /** {@inheritDoc} */
    public PropertyClass(String uid, String value) { super(uid, value); }

    /** {@inheritDoc} */
    @Override
    public Serializer<Class<?>> getSerializer() { return SERIALIZER; }

}
