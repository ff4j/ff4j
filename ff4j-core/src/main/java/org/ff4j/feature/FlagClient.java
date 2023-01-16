package org.ff4j.feature;

import org.ff4j.FF4jClient;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.namespace.NamespaceClient;
import org.ff4j.namespace.exception.NamespaceNotFoundException;

import java.util.Optional;
import java.util.function.Supplier;

/**
 * Working with feature client.
 *
 * @param ff4j
 *      ff4j reference
 * @param namespace
 *      namespace name
 * @param featureUid
 *      feature identifier
 */
public record FlagClient(FF4jClient ff4j, String namespace, String featureUid) implements Supplier<Flag> {

    /**
     * Fina a feature by its id.
     *
     * @return
     *      find a feature
     */
    public Optional<Flag> find() {
        return ff4j.getBackend().findFeatureById(namespace, featureUid);
    }

    /**
     * Return feature or exception if not exist.
     *
     * @return
     *      feature value
     */
    public Flag get() {
        Optional<Flag> optFeature = find();
        if (optFeature.isPresent()) return optFeature.get();
        if (!new NamespaceClient(ff4j, namespace).exist()) {
            throw new NamespaceNotFoundException(namespace);
        } else {
            throw new FeatureNotFoundException(namespace, featureUid);
        }
    }

    /**
     * Check if feature exist.
     *
     * @return
     *      validate feature existence
     */
    public boolean exist() {
        return ff4j.getBackend().existsFeature(namespace, featureUid);
    }

    /**
     * Delete current feature if exists.
     */
    public void delete() {
        get();
        ff4j.getBackend().deleteFeature(namespace, featureUid);
    }

    /**
     * Update a feature.
     *
     * @param f
     *      feature name
     */
    public void upsert(Flag f) {
        f.setUid(featureUid);
        ff4j.getBackend().saveFeature(namespace, f);
    }

}
