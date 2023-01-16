package org.ff4j;

import org.ff4j.backend.Backend;
import org.ff4j.backend.BackendTopology;
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.feature.Flag;
import org.ff4j.feature.exception.FeatureAlreadyExistException;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.namespace.NamespaceClient;
import org.ff4j.namespace.exception.NamespaceAlreadyException;
import org.ff4j.persistence.inmemory.BackendRepositoryInMemory;
import org.ff4j.property.Property;
import org.ff4j.property.exception.PropertyAlreadyExistException;
import org.ff4j.utils.Assert;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * Main class for FF4j.
 */
public class FF4jClient implements Predicate<String> {

    /** Logger for our Client. */
    private static final Logger LOGGER = LoggerFactory.getLogger(FF4jClient.class);

    /** default namespace. */
    public static final String DEFAULT_NAMESPACE = "default";
    
    /** Starting time, use to compute uptime. */
    private final Instant startTime = Instant.now();

    /** Hold configuration. */
    protected final FF4jClientConfiguration config;

    /** Backends with load-balancing and fail-over. */
    protected final BackendTopology backends;

    /** Local cache layer for db off-loading. */
    protected final FF4jCacheProxy cacheProxy;

    /** Shared context passed over strategies. */
    private final ThreadLocal<FF4jContext> context = new ThreadLocal<>();

    /**
     * Initialization with default configuration.
     * - no cache
     * - single in memory backend
     */
    public FF4jClient() {
        this(builder().withBackends(new Backend(new BackendRepositoryInMemory())));
        LOGGER.info("FF4j has been successfully initialized");
    }

    /**
     * Initialization with a Builder.
     *
     * @return
     *      configuration
     */
    public static FF4jClientConfiguration builder() {
        return new FF4jClientConfiguration();
    }

    /**
     * Initialization with provided configuration.
     *
     * @param conf
     *          configuration of the client
     */
    protected FF4jClient(FF4jClientConfiguration conf) {
        // Configuration reference
        this.config = conf;
        // Enforce user/password to each backend if provided at root level
        if ((conf.username != null) && (conf.password != null)) {
            conf.backendDeployment.values()
                .stream().flatMap(List::stream)
                .forEach(b -> b.setCredentials(conf.username, conf.password));
        }
        // Backends
        this.backends = new BackendTopology(conf.localDatacenter, conf.backendDeployment);
        // Backends Cache
        this.cacheProxy = (conf.getCache() != null) ? new FF4jCacheProxy(this, conf) : null;

        // Initialization of default
        if (!existNamespace(DEFAULT_NAMESPACE)) createDefaultNamespace();
    }

    /**
     * Retrieve all namespaces in the repository.
     *
     * @return
     *      namespace list
     */
    public Stream<String> namespaces() {
        return getBackend().findAllNamespaces();
    }

    /**
     * Access methods relative to namespaces with fluent.
     *
     * @param namespace
     *      current namespace
     * @return
     *      client
     */
    public NamespaceClient namespace(String namespace) {
        Assert.assertHasLength(namespace);
        return new NamespaceClient(this, namespace);
    }

    /**
     * Evaluate if a feature is toggled or not.
     *
     * @param namespace
     *     current namespace
     * @param feature
     *     feature identifier.
     * @return
     *     if feature is toggled
     */
    public boolean test(String namespace, String feature) {
        Assert.assertHasLength(namespace);
        Assert.assertHasLength(feature);
        return getBackend().test(namespace, feature);
    }

    /**
     * Evaluate if a feature is toggled or not.
     *
     * @param feature
     *     feature identifier.
     * @return
     *     if feature is toggled
     */
    @Override
    public boolean test(String feature) {
        return test(config.getNamespace(), feature);
    }

    /**
     * Test asynchronous Feature (toggle On / Off).
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      feature unique identifier
     * @return
     *      if feature should be toggled
     */
    public CompletionStage<Boolean> testAsync(String namespace, String uid) {
        return CompletableFuture.supplyAsync(() -> test(namespace, uid));
    }

    /**
     * Look up for a backend (or local cache).
     *
     * @return
     *      current backend
     */
    public Backend getBackend() {
        return (cacheProxy != null) ? cacheProxy: backends.lookupBackend().getResource();
    }

    /**
     * Access current namespace.
     *
     * @return
     *      selected namespace
     */
    public String getSelectedNamespace() {
        return Optional.ofNullable(this.config.namespace).orElse(DEFAULT_NAMESPACE);
    }

    /**
     * Gets start time
     *
     * @return value of start
     */
    public Instant getStartTime() {
        return startTime;
    }

    /**
     * Gets context
     *
     * @return value of context
     */
    public ThreadLocal<FF4jContext> getContext() {
        return context;
    }

    /**
     * Gets config
     *
     * @return value of config
     */
    public FF4jClientConfiguration getConfiguration() {
        return config;
    }

    // -------------------------------------
    // ---- Makes Features operations ------
    // -------------------------------------

    /**
     * Create expected objects in persistence.
     */
    public void createSchema() {
        getBackend().createSchema();
    }

    /**
     * Save current configuration.
     */
    public void saveConfiguration() {
        getBackend().saveConfiguration(this.config);
    }

    /**
     * List existing namespaces in the DB.
     *
     * @return
     *      list of namespaces
     */
    public Stream<String> findAllNamespaces() {
        return getBackend().findAllNamespaces();
    }

    /**
     * Create an empty namespace
     *
     * @param namespace
     *      namespace identifier
     * @throws NamespaceAlreadyException
     *      error if namespace exists
     * @return NamespaceAlreadyException
     *      self reference to chain operations
     */
    public FF4jClient createNamespace(String namespace) throws NamespaceAlreadyException {
        getBackend().createNamespace(namespace);
        return this;
    }

    /**
     * Create an empty namespace
     *
     * @return
     *      self reference to chain operations
     */
    public FF4jClient createDefaultNamespace() {
        return createNamespace(DEFAULT_NAMESPACE);
    }

    /**
     * Delete a namespace if exists.
     *
     * @param namespace
     *      target namespace
     * @return
     *      self reference to chain operations
     */
    public FF4jClient deleteNamespace(String namespace) {
        getBackend().deleteNamespace(namespace);
        return this;
    }

    /**
     * Test namespace existence.
     *
     * @param namespace
     *      target namespace
     * @return
     *      if the namespace exists
     */
    public boolean existNamespace(String namespace) {
        return getBackend().existNamespace(namespace);
    }

    /**
     * Find all features
     * @param namespace
     *      current namespace
     * @return
     *      all features
     */
    public Stream<Flag> findAllFeatures(String namespace) {
        return getBackend().findAllFeatures(namespace);
    }

    /**
     * Default namespace.
     *
     * @return
     *      list of namespaces
     */
    public Stream<Flag> findAllFeatures() {
        return findAllFeatures(getSelectedNamespace());
    }

    /**
     * Test feature existence.
     *
     * @param namespace
     *      current namespace
     * @param uid
     *      feature identifier
     * @return
     *      if feature exist
     */
    public boolean existsFeature(String namespace, String uid) {
        return getBackend().existsFeature(namespace, uid);
    }

    /**
     * Test feature existence.
     *
     * @param uid
     *      feature identifier
     * @return
     *      if feature exist
     */
    public boolean existsFeature(String uid) {
        return existsFeature(getSelectedNamespace(), uid);
    }

    /**
     * Create feature.
     *
     * @param feature
     *      feature
     * @param namespace
     *      namespace for the
     * @return
     *      current reference
     */
    public FF4jClient createFeature(String namespace, Flag feature)
    throws FeatureAlreadyExistException {
        getBackend().createFeature(namespace, feature);
        return this;
    }

    /**
     * Create feature in selected namespace.
     *
     * @param feature
     *      feature
     * @return
     *      current reference
     */
    public FF4jClient createFeature(Flag feature) throws FeatureAlreadyExistException {
        return createFeature(getSelectedNamespace(), feature);
    }

    /**
     * Create feature in selected namespace.
     *
     * @param features
     *      feature identifiers
     * @return
     *      current reference
     */
    public FF4jClient createFeatures(String... features) throws FeatureAlreadyExistException {
        Assert.assertNotNull(features);
        Arrays.stream(features).map(Flag::new).forEach(this::createFeature);
        return this;
    }

    /**
     * Save a feature.
     *
     * @param namespace
     *      namespace identifier.
     * @param feature
     *      feature identifier
     * @throws FeatureNotFoundException
     *      feature not present in namespace
     */
    public FF4jClient saveFeature(String namespace, Flag feature)
    throws FeatureNotFoundException {
        getBackend().saveFeature(namespace, feature);
        return this;
    }

    /**
     * Save a feature in current namespace.
     *
     * @param feature
     *      feature identifier
     * @throws FeatureNotFoundException
     *      feature not present in namespace
     */
    public FF4jClient saveFeature(Flag feature)
    throws FeatureNotFoundException {
        return saveFeature(getSelectedNamespace(), feature);
    }

    /**
     * Delete a feature.
     *
     * @param namespace
     *      namespace identifier.
     * @param uid
     *      feature identifier
     * @throws FeatureNotFoundException
     *      feature not present in namespace
     */
    public FF4jClient deleteFeature(String namespace, String uid) throws FeatureNotFoundException {
        getBackend().deleteFeature(namespace, uid);
        return this;
    }

    /**
     * Delete a feature in default namespace
     *
     * @param uid
     *      feature identifier
     * @throws FeatureNotFoundException
     *      feature not present in namespace
     */
    public FF4jClient deleteFeature(String uid)
    throws FeatureNotFoundException {
        return deleteFeature(getSelectedNamespace(), uid);
    }

    /**
     * Rename a feature.
     *
     * @param namespace
     *      namespace identifier.
     * @param uid
     *      feature identifier (new)
     * @param old
     *      feature identifier (old)
     * @throws FeatureNotFoundException
     *      feature not present in namespace
     */
    public FF4jClient renameFeature(String namespace, String old, String uid)
    throws FeatureNotFoundException {
        getBackend().renameFeature(namespace, old, uid);
        return this;
    }

    /**
     * Rename a feature in default namespace
     *
     * @param uid
     *      feature identifier (new)
     * @param old
     *      feature identifier (old)
     * @throws FeatureNotFoundException
     *      feature not present in namespace
     */
    public FF4jClient renameFeature(String old, String uid)
    throws FeatureNotFoundException {
        return renameFeature(getSelectedNamespace(), old, uid);
    }

    /**
     * Enable a feature.
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      feature identifier
     * @throws FeatureNotFoundException
     *      feature not present in namespace
     */
    public FF4jClient toggleOnFeature(String namespace, String uid)
    throws FeatureNotFoundException {
        getBackend().toggleOnFeature(namespace, uid);
        return this;
    }

    /**
     * Enable a feature in default namespace
     *
     * @param uid
     *      feature identifier
     * @throws FeatureNotFoundException
     *      feature not present in namespace
     */
    public FF4jClient toggleOnFeature(String uid)
    throws FeatureNotFoundException {
        return toggleOnFeature(getSelectedNamespace(), uid);
    }

    /**
     * Disable a feature.
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      feature identifier
     * @throws FeatureNotFoundException
     *      feature not present in namespace
     */
    public FF4jClient toggleOffFeature(String namespace, String uid)
    throws FeatureNotFoundException {
        getBackend().toggleOnFeature(namespace, uid);
        return this;
    }

    /**
     * Disable a feature in default namespace
     *
     * @param uid
     *      feature identifier
     * @throws FeatureNotFoundException
     *      feature not present in namespace
     */
    public FF4jClient toggleOffFeature(String uid)
    throws FeatureNotFoundException {
        return toggleOffFeature(getSelectedNamespace(), uid);
    }

    /**
     * Find a feature by its identifier
     *
     * @param namespace
     *      namespace identifier.
     * @param uid
     *      feature identifier (new)
     */
    public Optional<Flag> findFeatureById(String namespace, String uid) {
        return getBackend().findFeatureById(namespace, uid);
    }

    /**
     * Find a feature by its identifier in default namespace.
     *
     * @param uid
     *      feature identifier (new)
     */
    public Optional<Flag> findFeatureById(String uid) {
        return getBackend().findFeatureById(getSelectedNamespace(), uid);
    }

    /**
     * Find all properties in a namespace
     *
     * @return
     *      list of namespaces
     */
    public Stream<Property<?>> findAllProperties(String namespace) {
        return getBackend().findAllProperties(namespace);
    }

    /**
     * Find all properties in a selected namespace
     *
     * @return
     *      list of namespaces
     */
    public Stream<Property<?>> findAllProperties() {
        return findAllProperties(getSelectedNamespace());
    }

    /**
     * Test property existence.
     *
     * @param namespace
     *      current namespace
     * @param uid
     *      property identifier
     * @return
     *      if property exists
     */
    public boolean existsProperty(String namespace, String uid) {
        return getBackend().existsProperty(namespace, uid);
    }

    /**
     * Test property existence in selected namespace
     *
     * @param uid
     *      property identifier
     * @return
     *      if property exists
     */
    public boolean existsProperty(String uid) {
        return existsProperty(getSelectedNamespace(), uid);
    }

    /**
     * Create property.
     *
     * @param property
     *      property
     * @param namespace
     *      namespace identifier
     * @return
     *      current reference
     */
    public FF4jClient createProperty(String namespace, Property<?> property) throws PropertyAlreadyExistException {
        getBackend().createProperty(namespace, property);
        return this;
    }

    /**
     * Create property in select namespace.
     *
     * @param property
     *      property
     * @return
     *      current reference
     */
    public FF4jClient createProperty(Property<?> property) throws PropertyAlreadyExistException {
        return createProperty(getSelectedNamespace(), property);
    }

    /**
     * Save property.
     *
     * @param property
     *      property
     * @param namespace
     *      namespace identifier
     * @return
     *      current reference
     */
    public FF4jClient saveProperty(String namespace, Property<?> property) {
        getBackend().saveProperty(namespace, property);
        return this;
    }

    /**
     * Save property in selected namespace.
     *
     * @param property
     *      property
     * @return
     *      current reference
     */
    public FF4jClient saveProperty(Property<?> property) {
        return saveProperty(getSelectedNamespace(), property);
    }

    /**
     * Delete property.
     *
     * @param uid
     *      property identifier
     * @param namespace
     *      namespace identifier
     * @return
     *      current reference
     */
    public FF4jClient deleteProperty(String namespace, String uid) {
        getBackend().deleteProperty(namespace, uid);
        return this;
    }

    /**
     * Delete property.
     *
     * @param uid
     *      property identifier
     * @return
     *      current reference
     */
    public FF4jClient deleteProperty(String uid) {
        return deleteProperty(getSelectedNamespace(), uid);
    }

    /**
     * Rename a property.
     *
     * @param uid
     *      property identifier (new)
     * @param old
     *      property identifier (old)
     * @param namespace
     *      namespace identifier
     * @return
     *      current reference
     */
    public FF4jClient renameProperty(String namespace, String old, String uid) {
        getBackend().renameProperty(namespace, old, uid);
        return this;
    }

    /**
     * Rename a property in selected namespace.
     *
     * @param uid
     *      property identifier (new)
     * @param old
     *      property identifier (old)
     * @return
     *      current reference
     */
    public FF4jClient renameProperty(String old, String uid) {
        return renameProperty(getSelectedNamespace(), old, uid);
    }

    /**
     * Find a property by its id.
     *
     * @param uid
     *      property identifier
     * @param namespace
     *      namespace identifier
     * @return
     *      current reference
     */
    public Optional<Property<?>> findPropertyById(String namespace, String uid) {
        return getBackend().findPropertyById(namespace, uid);
    }

    /**
     * Find a property by its id in selected namespace
     *
     * @param uid
     *      property identifier
     * @return
     *      current reference
     */
    public Optional<Property<?>> findPropertyById(String uid) {
        return findPropertyById(getSelectedNamespace(), uid);
    }

}