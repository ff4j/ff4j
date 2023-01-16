package org.ff4j.persistence.inmemory;

import org.ff4j.FF4jClientConfiguration;
import org.ff4j.backend.BackendRepository;
import org.ff4j.feature.Flag;
import org.ff4j.feature.exception.FeatureAlreadyExistException;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.namespace.exception.NamespaceAlreadyException;
import org.ff4j.namespace.exception.NamespaceNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.exception.PropertyAlreadyExistException;
import org.ff4j.property.exception.PropertyNotFoundException;
import org.ff4j.utils.Assert;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Store Features, Properties and events in-memory.
 */
public class BackendRepositoryInMemory implements BackendRepository {

    /** Cached Feature Map */
    private final Map<String, NamespaceInMemory> namespaces = new HashMap<>();

    /** Hold Configuration. */
    private FF4jClientConfiguration configuration = new FF4jClientConfiguration();

    /**
     * Default constructor.
     */
    public BackendRepositoryInMemory() {}

    // --------------------------------------
    // ---- Work with Configuration ---------
    // --------------------------------------

    /** {@inheritDoc} */
    public void createSchema() {
        // nothing to do
    }

    /** {@inheritDoc} */
    @Override
    public FF4jClientConfiguration readConfiguration() {
        return configuration;
    }

    /** {@inheritDoc} */
    @Override
    public void saveConfiguration(FF4jClientConfiguration config) {
        this.configuration = config;
    }

    // -------------------------------------
    // ---- Work with Namespaces  ----------
    // -------------------------------------

    /** {@inheritDoc} */
    @Override
    public Stream<String> findAllNamespaces() {
        return namespaces.keySet().stream();
    }

    /** {@inheritDoc} */
    @Override
    public boolean existNamespace(String namespace) {
        Assert.assertHasLength(namespace);
        return namespaces.containsKey(namespace);
    }

    /** {@inheritDoc} */
    @Override
    public void createNamespace(String namespace) {
        if (existNamespace(namespace)) throw new NamespaceAlreadyException(namespace);
        namespaces.put(namespace, new NamespaceInMemory(namespace));
    }

    /** {@inheritDoc} */
    @Override
    public void deleteNamespace(String namespace) {
        if (!existNamespace(namespace)) throw new NamespaceNotFoundException(namespace);
        namespaces.remove(namespace);
    }

    // -------------------------------------
    // ---- Work with Features   -----------
    // -------------------------------------

    /** {@inheritDoc} */
    @Override
    public Stream<Flag> findAllFeatures(String namespace) {
        if (!existNamespace(namespace)) throw new NamespaceNotFoundException(namespace);
        return namespaces.get(namespace).getFeatures().values().stream();
    }

    /** {@inheritDoc} */
    @Override
    public boolean existsFeature(String namespace, String uid) {
        Assert.assertHasLength(uid);
        if (!existNamespace(namespace)) throw new NamespaceNotFoundException(namespace);
        return namespaces.get(namespace).getFeatures().containsKey(uid);
    }

    /** {@inheritDoc} */
    @Override
    public void createFeature(String namespace, Flag feature) throws FeatureAlreadyExistException {
        Assert.assertNotNull(feature);
        if (existsFeature(namespace, feature.getUid())) throw new FeatureAlreadyExistException(namespace, feature.getUid());
        namespaces.get(namespace).getFeatures().put(feature.getUid(), feature);
    }

    /** {@inheritDoc} */
    @Override
    public void saveFeature(String namespace, Flag feature) throws FeatureNotFoundException {
        if (!existNamespace(namespace)) throw new NamespaceNotFoundException(namespace);
        Assert.assertNotNull(feature);
        Assert.assertHasLength(feature.getUid());
        namespaces.get(namespace).getFeatures().put(feature.getUid(), feature);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteFeature(String namespace, String uid) throws FeatureNotFoundException {
        if (!existsFeature(namespace, uid)) throw new FeatureNotFoundException(namespace, uid);
        namespaces.get(namespace).getFeatures().remove(uid);
    }

    /** {@inheritDoc} */
    @Override
    public void renameFeature(String namespace, String old, String uid) throws FeatureNotFoundException {
        if (!existsFeature(namespace, old)) throw new FeatureNotFoundException(namespace, old);
        findFeatureById(namespace, old).ifPresent(feature -> {
            createFeature(namespace, feature.clone(uid));
            deleteFeature(namespace, old);
        });
    }

    /** {@inheritDoc} */
    @Override
    public Optional<Flag> findFeatureById(String namespace, String uid) {
        return existNamespace(namespace) ?
                Optional.ofNullable(namespaces.get(namespace).getFeatures().get(uid)) :
                Optional.empty();
    }

    // -------------------------------------
    // ---- Work with Properties -----------
    // -------------------------------------

    /** {@inheritDoc} */
    @Override
    public Stream<Property<?>> findAllProperties(String namespace) {
        if (!existNamespace(namespace)) throw new NamespaceNotFoundException(namespace);
        return namespaces.get(namespace).getProperties().values().stream();
    }

    /** {@inheritDoc} */
    @Override
    public boolean existsProperty(String namespace, String uid) {
        Assert.assertHasLength(uid);
        if (!existNamespace(namespace)) throw new NamespaceNotFoundException(namespace);
        return namespaces.get(namespace).getProperties().containsKey(uid);
    }

    /** {@inheritDoc} */
    @Override
    public void createProperty(String namespace, Property<?> property) throws PropertyAlreadyExistException {
        Assert.assertNotNull(property);
        if (existsProperty(namespace, property.getUid())) throw new PropertyAlreadyExistException(namespace, property.getUid());
        namespaces.get(namespace).getProperties().put(property.getUid(), property);
    }

    /** {@inheritDoc} */
    @Override
    public void saveProperty(String namespace, Property<?> property) {
        if (!existNamespace(namespace)) throw new NamespaceNotFoundException(namespace);
        Assert.assertNotNull(property);
        Assert.assertHasLength(property.getUid());
        namespaces.get(namespace).getProperties().put(property.getUid(), property);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String namespace, String uid) {
        if (!existsProperty(namespace, uid)) throw new PropertyNotFoundException(namespace, uid);
        namespaces.get(namespace).getProperties().remove(uid);
    }

    /** {@inheritDoc} */
    @Override
    public Optional<Property<?>> findPropertyById(String namespace, String uid) {
        return existNamespace(namespace) ?
                Optional.ofNullable(namespaces.get(namespace).getProperties().get(uid)) :
                Optional.empty();
    }

    /** {@inheritDoc} */
    @Override
    public void renameProperty(String namespace, String old, String uid) {
        if (!existsProperty(namespace, uid)) throw new PropertyNotFoundException(namespace, uid);
        findPropertyById(namespace, old).ifPresent(property -> {
            deleteProperty(namespace, old);
            createProperty(namespace, property);
        });
    }

}
