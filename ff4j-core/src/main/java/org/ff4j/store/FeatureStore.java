package org.ff4j.store;

import java.util.Map;

import org.ff4j.Feature;

/**
 * Represents a Backing store for {@link Feature}
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface FeatureStore {

    /**
     * Enable/UP/switch on a FlipPoint.
     * 
     * @param featureID
     *            unique feature identifier
     */
    void enable(String featureID);

    /**
     * Create Feature is does not exist and enable it.
     * 
     * @param featureID
     *            unique feature identifier
     */
    void disable(String fId);

    /**
     * Check if Feature Exist.
     * 
     * @return unique feature identifier
     */
    boolean exist(String featId);

    /**
     * Create flipPoint in storage (with roles).
     * 
     * @param fp
     *            create roles
     */
    void create(Feature fp);

    /**
     * Create Feature is does not exist and enable it.
     * 
     * @param featureID
     *            unique feature identifier
     */
    Feature read(String featureUid);

    /**
     * Access to all features to get information.
     * 
     * @return all features
     */
    Map<String, Feature> readAll();

    /**
     * Remove fliPoint from store.
     * 
     * @param fp
     *            flipPoint
     */
    void delete(String fpId);

    /**
     * Update FlipPoint within store.
     * 
     * @param fp
     *            flipPoint new element
     */
    void update(Feature fp);

    /**
     * Add a role to a flipPOINT.
     * 
     * @param roleName
     */
    void grantRoleOnFeature(String flipId, String roleName);

    /**
     * Remove role to acess flip point
     * 
     * @param roleName
     */
    void removeRoleFromFeature(String flipId, String roleName);

}
