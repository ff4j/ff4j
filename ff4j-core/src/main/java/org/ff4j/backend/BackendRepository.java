package org.ff4j.backend;

import org.ff4j.FF4jConfiguration;
import org.ff4j.feature.Feature;
import org.ff4j.feature.exception.FeatureFlagAlreadyExistException;
import org.ff4j.feature.exception.FeatureFlagNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.exception.PropertyAlreadyExistException;

import java.util.Optional;
import java.util.stream.Stream;

/**
 * Interface to interact with FF4j persistence.
 */
public interface BackendRepository extends BackendRepositoryReadOnly {

    // -------------------------------------
    // -------  Initialization  ------------
    // -------------------------------------

    /**
     * Create schema for features and properties.
     */
    void createSchema();

    /**
     * Read configuration from backend.
     *
     * @return
     *      ff4j client configuration
     */
    FF4jConfiguration readConfiguration();

    /**
     * Update configuration.
     *
     * @param config
     *      updated configuration
     */
    void saveConfiguration(FF4jConfiguration config);

    // -------------------------------------
    // ---- Work with Workspaces -----------
    // -------------------------------------

    /**
     * Create a workspace.
     *
     * @param workspace
     *      workspace identifier
     */
    void createWorkspace(String workspace);

    /**
     * Delete a workspace.
     *
     * @param workspace
     *      workspace identifier
     */
    void deleteWorkspace(String workspace);

    // -------------------------------------
    // ---- Work with Features   -----------
    // -------------------------------------

    /**
     * List all features in a workspace.
     *
     * @param workspace
     *         workspace name
     * @return all features
     */
    Stream<Feature> getFeatures(String workspace);

    /**
     * List all features in a workspace.
     *
     * @return all features
     */
    default Stream<Feature> getFeatures() {
        return getFeatures(getCurrentWorkspace());
    }

    /**
     * List features names in a workspace.
     *
     * @param workspace
     *         workspace name
     * @return all features names
     */
    Stream<String> getFeaturesNames(String workspace);

    /**
     * List features names in a workspace.
     *
     * @return all features names
     */
    default Stream<String> getFeaturesNames() {
        return getFeaturesNames(getCurrentWorkspace());
    }

    /**
     * Find a feature from its id.
     *
     * @param workspace
     *         workspace identifier
     * @param uid
     *         feature identifier
     * @return feature object if exist
     */
    Optional<Feature> findFeature(String workspace, String uid);

    /**
     * Find a feature from its id.
     *
     * @param uid
     *         feature identifier
     * @return feature object if exist
     */
    default Optional<Feature> findFeature(String uid) {
        return findFeature(getCurrentWorkspace(), uid);
    }

    /**
     * Find a feature from its id.
     *
     * @param uid
     *         feature identifier
     * @return feature object if exist
     */
    default Feature getFeature(String uid) {
        return findFeature(uid).orElseThrow(() -> new FeatureFlagNotFoundException(getCurrentWorkspace(), uid));
    }

    /**
     * Find a feature from its id.
     *
     * @param workspace
     *         workspace identifier
     * @param uid
     *         feature identifier
     * @return feature object if exist
     */
    default Feature getFeature(String workspace, String uid) {
        return findFeature(workspace, uid).orElseThrow(() -> new FeatureFlagNotFoundException(workspace, uid));
    }

    /**
     * Create a flag.
     *
     * @param workspace
     *      workspace identifier
     * @param flag
     *      flag object
     * @exception FeatureFlagAlreadyExistException
     *      the flag was already existing
     */
    void createFeature(String workspace, Feature flag)
    throws FeatureFlagAlreadyExistException;

    /**
     * Upsert a flag.
     *
     * @param workspace
     *      workspace identifier
     * @param flag
     *      flag object
     * @exception FeatureFlagNotFoundException
     *      the flag has not been found.
     */
    void saveFeature(String workspace, Feature flag)
    throws FeatureFlagNotFoundException;

    /**
     * Delete a feature.
     *
     * @param workspace
     *      workspace identifier
     * @param uid
     *      feature identifier
     * @exception FeatureFlagNotFoundException
     *      the feature has not been found.
     */
    void deleteFeature(String workspace, String uid)
    throws FeatureFlagNotFoundException;

    /**
     * Rename a feature.
     *
     * @param workspace
     *      workspace identifier
     * @param old
     *      previous feature identifier
     * @param uid
     *      new feature identifier
     * @exception FeatureFlagNotFoundException
     *      the feature has not been found.
     */
    void renameFeature(String workspace, String old, String uid)
    throws FeatureFlagNotFoundException;

    /**
     * ToggleOn a feature within a workspace.
     *
     * @param workspace
     *      workspace name
     * @param flag
     *      feature identifier
     * @exception FeatureFlagNotFoundException
     *      the feature has not been found.
     */
    default void toggleOn(String workspace, String flag)
    throws FeatureFlagNotFoundException {
        findFeature(workspace, flag).ifPresentOrElse(
                f -> saveFeature(workspace, f.toggleOn()),
                () -> { throw new FeatureFlagNotFoundException(workspace, flag); } );
    }

    /**
     * Toggle Off a feature within a workspace.
     *
     * @param workspace
     *      workspace name
     * @param uid
     *      feature identifier
     * @exception FeatureFlagNotFoundException
     *      the feature has not been found.
     */
    default void toggleOff(String workspace, String uid)
    throws FeatureFlagNotFoundException {
        findFeature(workspace, uid).ifPresentOrElse(
                f -> saveFeature(workspace, f.toggleOff()),
                () -> { throw new FeatureFlagNotFoundException(workspace, uid); } );
    }

    // -------------------------------------
    // ---- Work with Properties -----------
    // -------------------------------------

    /**
     * List features names in a workspace.
     *
     * @param workspace
     *         workspace name
     * @return all property names
     */
    Stream<String> getPropertiesNames(String workspace);

    /**
     * List features names in a workspace.
     *
     * @return all property names
     */
    default Stream<String> getPropertiesNames() {
        return getPropertiesNames(getCurrentWorkspace());
    }

    /**
     * Create a feature.
     *
     * @param workspace
     *      workspace identifier
     * @param property
     *      property object
     * @exception PropertyAlreadyExistException
     *      the property was already existing
     */
    void createProperty(String workspace, Property<?> property)
    throws PropertyAlreadyExistException;

    /**
     * Upsert a property.
     *
     * @param workspace
     *      workspace identifier
     * @param property
     *      property object
     */
    void saveProperty(String workspace, Property<?> property);

    /**
     * Delete a property.
     *
     * @param workspace
     *      workspace identifier
     * @param uid
     *      property identifier
     */
    void deleteProperty(String workspace, String uid);

    /**
     * Rename a property.
     *
     * @param workspace
     *      workspace identifier
     * @param old
     *      previous property identifier
     * @param uid
     *      new property identifier
     */
    void renameProperty(String workspace, String old, String uid);

}
