package org.ff4j.property.serialize;

import org.ff4j.exception.SerializationException;
import org.ff4j.property.Property;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Serializing a list.
 *
 * @param <TYPE>
 *       scalar type
 * @param <PROPERTY>
 *       property for a simple type
 */
public class GenericListSerializer<TYPE, PROPERTY extends Property<TYPE>> implements Serializer<List<TYPE>> {

    /** Use a marker to serialize a LIST. */
    public static final String LIST_SEPARATOR = ",";

    /** temp property to access its serializer. */
    private PROPERTY property;

    /**
     * Default Constructor.
     *
     * @param scalarType
     *      current type
     */
    public GenericListSerializer(Class<PROPERTY> scalarType) {
        try {
            this.property = scalarType.getConstructor(String.class).newInstance("tmp");
        } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | InstantiationException e) {
            throw new IllegalArgumentException("Cannot initialize serializer", e);
        }
    }

    @Override
    public List<TYPE> deserialize(String expression) {
        if (expression == null) return null;
        if (expression.startsWith("[") && expression.length() > 1) {
            expression = expression.substring(1, expression.length()-1);
        }
        return Arrays.stream(expression.split(LIST_SEPARATOR))
                .map(String::trim)
                .map(property.getSerializer()::deserialize)
                .collect(Collectors.toList());
    }

    @Override
    public String serialize(List<TYPE> collection) {
        try {
            if (collection == null) return null;
            return String.join(LIST_SEPARATOR, collection
                    .stream()
                    .map(property.getSerializer()::serialize)
                    .toList());
        } catch(RuntimeException re) {
            throw new SerializationException(collection);
        }
    }

}
