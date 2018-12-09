package org.ff4j.mapper;

import org.ff4j.security.FF4jGrantees;

/**
 * MAP from repository.
 *
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <REQ>
 * @param <RES>
 */
public interface FeaturePermissionMapper <REQ , RES> extends Mapper < FF4jGrantees, REQ, RES > {}