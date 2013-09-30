package org.ff4j.web.store;

import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.store.FeatureStore;

/**
 * Implementation of {@link FeatureStore} to access features through HTTP.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class RemoteHttpFeatureStore implements FeatureStore {

    @Override
    public void enable(String featureID) {
        // TODO Auto-generated method stub

    }

    @Override
    public void disable(String fId) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean exist(String featId) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void create(Feature fp) {
        // TODO Auto-generated method stub

    }

    @Override
    public Feature read(String featureUid) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Map<String, Feature> readAll() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void delete(String fpId) {
        // TODO Auto-generated method stub

    }

    @Override
    public void update(Feature fp) {
        // TODO Auto-generated method stub

    }

    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        // TODO Auto-generated method stub

    }

    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        // TODO Auto-generated method stub

    }

    // TODO Implementation HTTP of Feature Store

}
