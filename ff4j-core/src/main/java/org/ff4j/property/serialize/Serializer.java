package org.ff4j.property.serialize;

/**
 * Convert from String bean and vice-versa.
 *
 * @param <T>
 *     type to work with
 */
public interface Serializer<T> {

    /**
     * Build type back.
     *
     * @param expression
     *      String expression
     * @return
     *      object
     */
    T deserialize(String expression);

    /**
     * Serialize as String
     *
     * @param object
     *      object expression
     * @return
     *      string object
     */
    String serialize(T object);

}
