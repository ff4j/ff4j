package org.ff4j.v1.mapper;

import org.ff4j.v1.core.Feature;

/**
 * Specialization of the interface.
 *
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <STORE_OBJ>
 *      target driver object.
 */
public interface FeatureMapper <STORE_OBJ> extends Mapper< Feature, STORE_OBJ> {}
