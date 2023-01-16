package org.ff4j.backend;

import org.ff4j.FF4jClientConfiguration;
import org.ff4j.feature.Flag;
import org.ff4j.feature.exception.FeatureAlreadyExistException;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.exception.PropertyAlreadyExistException;

import java.util.Optional;
import java.util.stream.Stream;

/**
 * Interface to interact with FF4j persistence.
 */
public interface BackendRepository {

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
    FF4jClientConfiguration readConfiguration();

    /**
     * Update configuration.
     *
     * @param config
     *      updated configuration
     */
    void saveConfiguration(FF4jClientConfiguration config);

    // -------------------------------------
    // ---- Work with Namespaces -----------
    // -------------------------------------

    /**
     * List available namespaces.
     *
     * @return
     *      namespace identifiers
     */
    Stream<String> findAllNamespaces();

    /**
     * Create a namespace.
     *
     * @param namespace
     *      namespace identifier
     */
    void createNamespace(String namespace);

    /**
     * Delete a namespace.
     *
     * @param namespace
     *      namespace identifier
     */
    void deleteNamespace(String namespace);

    /**
     * Check existence of a namespace.
     *
     * @param namespace
     *      namespace identifier
     * @return
     *      if namespace exists
     */
    boolean existNamespace(String namespace);

    // -------------------------------------
    // ---- Work with Features   -----------
    // -------------------------------------

    /**
     * List all features in a namespace.
     *
     * @param namespace
     *       namespace name
     * @return
     *      all features
     */
    Stream<Flag> findAllFeatures(String namespace);

    /**
     * Check existence of a feature.
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      feature identifier
     * @return
     *      if feature exists
     */
    boolean existsFeature(String namespace, String uid);

    /**
     * Create a feature.
     *
     * @param namespace
     *      namespace identifier
     * @param feature
     *      feature object
     * @exception FeatureAlreadyExistException
     *      the feature was already existing
     */
    void createFeature(String namespace, Flag feature)
    throws FeatureAlreadyExistException;

    /**
     * Upsert a feature.
     *
     * @param namespace
     *      namespace identifier
     * @param feature
     *      feature object
     * @exception FeatureNotFoundException
     *      the feature has not been found.
     */
    void saveFeature(String namespace, Flag feature)
    throws FeatureNotFoundException;

    /**
     * Delete a feature.
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      feature identifier
     * @exception FeatureNotFoundException
     *      the feature has not been found.
     */
    void deleteFeature(String namespace, String uid)
    throws FeatureNotFoundException;

    /**
     * Rename a feature.
     *
     * @param namespace
     *      namespace identifier
     * @param old
     *      previous feature identifier
     * @param uid
     *      new feature identifier
     * @exception FeatureNotFoundException
     *      the feature has not been found.
     */
    void renameFeature(String namespace, String old, String uid)
    throws FeatureNotFoundException;

    /**
     * Find a feature from its id.
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      feature identifier
     * @return
     *      feature object if exist
     */
    Optional<Flag> findFeatureById(String namespace, String uid);

    /**
     * ToggleOn a feature within a namespace.
     *
     * @param namespace
     *      namespace name
     * @param uid
     *      feature identifier
     * @exception FeatureNotFoundException
     *      the feature has not been found.
     */
    default void toggleOnFeature(String namespace, String uid)
    throws FeatureNotFoundException {
        findFeatureById(namespace, uid).ifPresentOrElse(
                f -> saveFeature(namespace, f.toggleOn()),
                () -> { throw new FeatureNotFoundException(namespace, uid); } );
    }

    /**
     * Toggle Off a feature within a namespace.
     *
     * @param namespace
     *      namespace name
     * @param uid
     *      feature identifier
     * @exception FeatureNotFoundException
     *      the feature has not been found.
     */
    default void toggleOffFeature(String namespace, String uid)
    throws FeatureNotFoundException {
        findFeatureById(namespace, uid).ifPresentOrElse(
                f -> saveFeature(namespace, f.toggleOff()),
                () -> { throw new FeatureNotFoundException(namespace, uid); } );
    }

    // -------------------------------------
    // ---- Work with Properties -----------
    // -------------------------------------

    /**
     * List all properties in a namespace.
     *
     * @param namespace
     *       namespace name
     * @return
     *      all properties
     */
    Stream<Property<?>> findAllProperties(String namespace);

    /**
     * Check existence of a property.
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      property identifier
     * @return
     *      if property exists
     */
    boolean existsProperty(String namespace, String uid);

    /**
     * Create a feature.
     *
     * @param namespace
     *      namespace identifier
     * @param property
     *      property object
     * @exception PropertyAlreadyExistException
     *      the property was already existing
     */
    void createProperty(String namespace, Property<?> property)
    throws PropertyAlreadyExistException;

    /**
     * Upsert a property.
     *
     * @param namespace
     *      namespace identifier
     * @param property
     *      property object
     */
    void saveProperty(String namespace, Property<?> property);

    /**
     * Delete a property.
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      property identifier
     */
    void deleteProperty(String namespace, String uid);

    /**
     * Rename a property.
     *
     * @param namespace
     *      namespace identifier
     * @param old
     *      previous property identifier
     * @param uid
     *      new property identifier
     */
    void renameProperty(String namespace, String old, String uid);

    /**
     * Find a property from its id.
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      property identifier
    * @param <T>
     *      property value
     * @return
     *      property object if exist

     * @return
     */
    Optional<Property<?>> findPropertyById(String namespace, String uid);



}
