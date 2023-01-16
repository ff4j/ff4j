package org.ff4j.namespace;

import org.ff4j.FF4jClient;
import org.ff4j.feature.Flag;
import org.ff4j.feature.FlagClient;
import org.ff4j.property.Property;
import org.ff4j.utils.Assert;

import java.util.stream.Stream;

/**
 * Interacting with namespaces as a fluent API.
 */
public record NamespaceClient(FF4jClient ff4j, String namespace) {

    /**
     * Test existence.
     *
     * @return namespace existence
     */
    public boolean exist() {
        return ff4j.existNamespace(namespace);
    }

    /**
     * Create the namespace
     */
    public void create() { ff4j.createNamespace(namespace); }

    /**
     * Delete namespace
     */
    public void delete() { ff4j.deleteNamespace(namespace); }

    /**
     * List features for a namespace.
     *
     * @return list of features
     */
    public Stream<Flag> features() {
        return ff4j.getBackend().findAllFeatures(namespace);
    }

    /**
     * List features for a namespace.
     *
     * @return list of features
     */
    public Stream<Property<?>> properties() {
        return ff4j.getBackend().findAllProperties(namespace);
    }

    /**
     * Access a feature in particular.
     *
     * @param uid feature identifier
     * @return client
     */
    public FlagClient feature(String uid) {
        Assert.assertHasLength(uid);
        return new FlagClient(ff4j, namespace, uid);
    }

}
