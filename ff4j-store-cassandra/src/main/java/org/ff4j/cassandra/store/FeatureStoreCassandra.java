package org.ff4j.cassandra.store;

import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.store.AbstractFeatureStore;

/**
 * Implementation of {@link FeatureStore} to work with Cassandra Storage.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class FeatureStoreCassandra extends AbstractFeatureStore {

    /** {@inheritDoc} */
    @Override
    public void enable(String featureID) {
        throw new UnsupportedOperationException("Not yet ready");
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String fId) {
        throw new UnsupportedOperationException("Not yet ready");
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        throw new UnsupportedOperationException("Not yet ready");
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        throw new UnsupportedOperationException("Not yet ready");
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featureUid) {
        throw new UnsupportedOperationException("Not yet ready");
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String fpId) {
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String featureId, String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
    }
}
