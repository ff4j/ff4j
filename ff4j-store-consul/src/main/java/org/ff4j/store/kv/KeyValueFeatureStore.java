package org.ff4j.store.kv;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;

/**
 * Superclass to work with Key/Value Stores.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class KeyValueFeatureStore < D extends KeyValueDriver > extends AbstractFeatureStore {

    /** Driver to access a K/V Store. */
    protected D driver;
    
    /**
     * Default constructor
     */
    public KeyValueFeatureStore() {
    }
            
    /**
     * Work with Key-Value.
     *
     * @param driver
     *      target driver
     */
    public KeyValueFeatureStore(D driver) {
        this.driver = driver;
    }
        
    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertParamHasLength(uid, "Feature identifier");
        return getDriver().existKey(getDriver().getFeatureKey(uid));
    }
    
    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        // Read from redis, feature not found if no present
        Feature f = read(uid);
        // Update within Object
        f.enable();
        // Serialization and update key, update TTL
        update(f);
    }
    
    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        // Read from redis, feature not found if no present
        Feature f = read(uid);
        // Update within Object
        f.disable();
        // Serialization and update key, update TTL
        update(f);
    }
    
    /** {@inheritDoc} */
    @Override
    public void update(Feature feature) {
        if (feature == null) {
            throw new IllegalArgumentException("Feature cannot be null");
        }
        if (!exist(feature.getUid())) {
            throw new FeatureNotFoundException(feature.getUid());
        }
        driver.putValue(getDriver().getFeatureKey(feature.getUid()), feature.toJson());
    }
    
    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        return FeatureJsonParser.parseFeature(
                getDriver().getValue(getDriver().getFeatureKey(uid)));
    }
    
    /** {@inheritDoc} */
    @Override
    public void create(Feature feature) {
        if (feature == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (exist(feature.getUid())) {
            throw new FeatureAlreadyExistException(feature.getUid());
        }
        getDriver().putValue(getDriver().getFeatureKey(feature.getUid()), feature.toJson());
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getDriver().deleteKey(getDriver().getFeatureKey(uid));
    }  
    
    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        Util.assertParamHasLength(roleName, "roleName (#2)");
        // retrieve
        Feature f = read(flipId);
        // modify
        f.getPermissions().add(roleName);
        // persist modification
        update(f);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        Util.assertParamHasLength(roleName, "roleName (#2)");
        // retrieve
        Feature f = read(flipId);
        f.getPermissions().remove(roleName);
        // persist modification
        update(f);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        Util.assertParamHasLength(groupName, "groupName");
        Map < String, Feature > features = readAll();
        Map < String, Feature > group = new HashMap<>();
        for (Map.Entry<String,Feature> uid : features.entrySet()) {
            if (groupName.equals(uid.getValue().getGroup())) {
                group.put(uid.getKey(), uid.getValue());
            }
        }
        if (group.isEmpty()) {
            throw new GroupNotFoundException(groupName);
        }
        return group;
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertParamHasLength(groupName, "groupName");
        Map < String, Feature > features = readAll();
        Map < String, Feature > group = new HashMap<>();
        for (Map.Entry<String,Feature> uid : features.entrySet()) {
            if (groupName.equals(uid.getValue().getGroup())) {
                group.put(uid.getKey(), uid.getValue());
            }
        }
        return !group.isEmpty();
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        Map < String, Feature > features = readGroup(groupName);
        for (Map.Entry<String,Feature> uid : features.entrySet()) {
            uid.getValue().enable();
            update(uid.getValue());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        Map < String, Feature > features = readGroup(groupName);
        for (Map.Entry<String,Feature> uid : features.entrySet()) {
            uid.getValue().disable();
            update(uid.getValue());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String featureId, String groupName) {
        Util.assertParamHasLength(groupName, "groupName (#2)");
        // retrieve
        Feature f = read(featureId);
        f.setGroup(groupName);
        // persist modification
        update(f);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
        Util.assertParamHasLength(groupName, "groupName (#2)");
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        // retrieve
        Feature f = read(featureId);
        f.setGroup(null);
        // persist modification
        update(f);
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        Map < String, Feature > features = readAll();
        Set < String > groups = new HashSet<>();
        for (Map.Entry<String,Feature> uid : features.entrySet()) {
            groups.add(uid.getValue().getGroup());
        }
        groups.remove(null);
        return groups;
    }

    /**
     * Getter accessor for attribute 'driver'.
     *
     * @return
     *       current value of 'driver'
     */
    public D getDriver() {
        if (driver == null) {
            throw new IllegalStateException("Cannot access target K/V store");
        }
        return driver;
    }

    /**
     * Setter accessor for attribute 'driver'.
     * @param driver
     * 		new value for 'driver '
     */
    public void setDriver(D driver) {
        this.driver = driver;
    }
}
