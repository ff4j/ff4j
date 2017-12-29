package org.ff4j.v1.mapper;

import org.ff4j.v1.property.Property;

/**
 * Specialization of {@link Mapper} for {@link Property}.
 *
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <STORE_OBJ>
 *      target database driver bean
 */
public interface PropertyMapper <STORE_OBJ > extends Mapper< Property<?>, STORE_OBJ>{}
