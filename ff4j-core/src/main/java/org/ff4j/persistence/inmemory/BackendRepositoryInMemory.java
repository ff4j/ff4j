package org.ff4j.persistence.inmemory;

import org.ff4j.FF4jConfiguration;
import org.ff4j.backend.BackendRepository;
import org.ff4j.feature.Feature;
import org.ff4j.feature.exception.FeatureFlagAlreadyExistException;
import org.ff4j.feature.exception.FeatureFlagNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.evaluate.FF4jEvaluationContext;
import org.ff4j.property.exception.PropertyAlreadyExistException;
import org.ff4j.property.exception.PropertyNotFoundException;
import org.ff4j.utils.Assert;
import org.ff4j.workspace.exception.WorkspaceAlreadyException;
import org.ff4j.workspace.exception.WorkspaceNotFoundException;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Store Features, Properties and events in-memory.
 */
public class BackendRepositoryInMemory implements BackendRepository {

    /** Cached Feature Map */
    private final Map<String, Workspace> namespaces = new HashMap<>();

    /** Hold Configuration. */
    private FF4jConfiguration configuration = new FF4jConfiguration();

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
    public FF4jConfiguration readConfiguration() {
        return configuration;
    }

    /** {@inheritDoc} */
    @Override
    public void saveConfiguration(FF4jConfiguration config) {
        this.configuration = config;
    }

    // -------------------------------------
    // ---- Work with Namespaces  ----------
    // -------------------------------------

    /** {@inheritDoc} */
    public Stream<String> findAllWorkspaces() {
        return namespaces.keySet().stream();
    }

    /**
     * Test workspace existence.
     * @param namespace
     * @return
     */
    private boolean existWorkspace(String namespace) {
        Assert.assertHasLength(namespace);
        return namespaces.containsKey(namespace);
    }

    /** {@inheritDoc} */
    @Override
    public void createWorkspace(String namespace) {
        if (existWorkspace(namespace)) throw new WorkspaceAlreadyException(namespace);
        namespaces.put(namespace, new Workspace(namespace));
    }

    /** {@inheritDoc} */
    @Override
    public void deleteWorkspace(String namespace) {
        if (!existWorkspace(namespace)) throw new WorkspaceNotFoundException(namespace);
        namespaces.remove(namespace);
    }

    @Override
    public Stream<Feature> getFeatures(String workspace) {
        return null;
    }

    @Override
    public Stream<String> getFeaturesNames(String workspace) {
        return null;
    }

    @Override
    public Optional<Feature> findFeature(String workspace, String uid) {
        return Optional.empty();
    }

    // -------------------------------------
    // ---- Work with Features   -----------
    // -------------------------------------

    /** {@inheritDoc} */

    public Stream<Feature> findAllFeatureFlags(String workspace) {
        if (!existWorkspace(workspace)) throw new WorkspaceNotFoundException(workspace);
        return namespaces.get(workspace).getFeatures().values().stream();
    }

    /** {@inheritDoc} */

    public boolean existsFeatureFlag(String workspace, String uid) {
        Assert.assertHasLength(uid);
        if (!existWorkspace(workspace)) throw new WorkspaceNotFoundException(workspace);
        return namespaces.get(workspace).getFeatures().containsKey(uid);
    }

    /** {@inheritDoc} */
    @Override
    public void createFeature(String workspace, Feature flag) throws FeatureFlagAlreadyExistException {
        Assert.assertNotNull(flag);
        if (existsFeatureFlag(workspace, flag.getUid())) throw new FeatureFlagAlreadyExistException(workspace, flag.getUid());
        namespaces.get(workspace).getFeatures().put(flag.getUid(), flag);
    }

    @Override
    public void saveFeature(String workspace, Feature flag) throws FeatureFlagNotFoundException {

    }

    /** {@inheritDoc} */

    public void saveFeatureFlag(String workspace, Feature flag) throws FeatureFlagNotFoundException {
        if (!existWorkspace(workspace)) throw new WorkspaceNotFoundException(workspace);
        Assert.assertNotNull(flag);
        Assert.assertHasLength(flag.getUid());
        namespaces.get(workspace).getFeatures().put(flag.getUid(), flag);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteFeature(String workspace, String uid) throws FeatureFlagNotFoundException {
        if (!existsFeatureFlag(workspace, uid)) throw new FeatureFlagNotFoundException(workspace, uid);
        namespaces.get(workspace).getFeatures().remove(uid);
    }

    /** {@inheritDoc} */
    @Override
    public void renameFeature(String workspace, String old, String uid) throws FeatureFlagNotFoundException {
        if (!existsFeatureFlag(workspace, old)) throw new FeatureFlagNotFoundException(workspace, old);
        findFeatureFlagById(workspace, old).ifPresent(feature -> {
            createFeature(workspace, feature.clone(uid));
            deleteFeature(workspace, old);
        });
    }

    @Override
    public Stream<String> getPropertiesNames(String workspace) {
        return null;
    }

    /** {@inheritDoc} */

    public Optional<Feature> findFeatureFlagById(String workspace, String uid) {
        return existWorkspace(workspace) ?
                Optional.ofNullable(namespaces.get(workspace).getFeatures().get(uid)) :
                Optional.empty();
    }

    // -------------------------------------
    // ---- Work with Properties -----------
    // -------------------------------------

    /** {@inheritDoc} */
    public Stream<Property<?>> findAllProperties(String workspace) {
        if (!existWorkspace(workspace)) throw new WorkspaceNotFoundException(workspace);
        return namespaces.get(workspace).getProperties().values().stream();
    }

    /** {@inheritDoc} */
    public boolean existsProperty(String workspace, String uid) {
        Assert.assertHasLength(uid);
        if (!existWorkspace(workspace)) throw new WorkspaceNotFoundException(workspace);
        return namespaces.get(workspace).getProperties().containsKey(uid);
    }

    /** {@inheritDoc} */
    @Override
    public void createProperty(String workspace, Property<?> property) throws PropertyAlreadyExistException {
        Assert.assertNotNull(property);
        if (existsProperty(workspace, property.getUid())) throw new PropertyAlreadyExistException(workspace, property.getUid());
        namespaces.get(workspace).getProperties().put(property.getUid(), property);
    }

    /** {@inheritDoc} */
    @Override
    public void saveProperty(String workspace, Property<?> property) {
        if (!existWorkspace(workspace)) throw new WorkspaceNotFoundException(workspace);
        Assert.assertNotNull(property);
        Assert.assertHasLength(property.getUid());
        namespaces.get(workspace).getProperties().put(property.getUid(), property);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String workspace, String uid) {
        if (!existsProperty(workspace, uid)) throw new PropertyNotFoundException(workspace, uid);
        namespaces.get(workspace).getProperties().remove(uid);
    }

    /** {@inheritDoc} */

    public Optional<Property<?>> findPropertyById(String workspace, String uid) {
        return existWorkspace(workspace) ?
                Optional.ofNullable(namespaces.get(workspace).getProperties().get(uid)) :
                Optional.empty();
    }

    /** {@inheritDoc} */
    @Override
    public void renameProperty(String workspace, String old, String uid) {
        if (!existsProperty(workspace, uid)) throw new PropertyNotFoundException(workspace, uid);
        findPropertyById(workspace, old).ifPresent(property -> {
            deleteProperty(workspace, old);
            createProperty(workspace, property);
        });
    }

    @Override
    public Stream<String> getWorkspaces() {
        return null;
    }

    @Override
    public Map<String, Boolean> getFeatureFlags(String workspace, FF4jEvaluationContext context) {
        return null;
    }

    @Override
    public Optional<Boolean> findFeatureFlag(String workspace, String uid, FF4jEvaluationContext context) {
        return Optional.empty();
    }

    @Override
    public Stream<Property<?>> getProperties(String workspace) {
        return null;
    }

    @Override
    public Optional<Property<?>> findProperty(String workspace, String uid) {
        return Optional.empty();
    }
}
