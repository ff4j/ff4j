package org.ff4j.security;

import org.ff4j.security.FF4jPermission;

import java.util.stream.Stream;

/**
 * Hold permissions on entities for fine-grained usage.
 */
public interface AuthorizationRepository {

    /**
     * Retrieve permissions at namespace level
     *
     * @param namespace
     *         current namespace
     * @return
     *          permissions
     */
    Stream<FF4jPermission> findNamespacePermissions(String namespace);

    /**
     * Retrieve permissions at feature level.
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      feature identifier
     * @return
     *      permissions
     */
    Stream<FF4jPermission> findFeaturePermissions(String namespace, String uid);

    /**
     * Retrieve permissions at property level.
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      feature identifier
     * @return
     *      permissions
     */
    Stream<FF4jPermission> findPropertyPermission(String namespace, String uid);

}
