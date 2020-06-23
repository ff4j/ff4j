package org.ff4j.store;

/*
 * #%L
 * ff4j-store-redis
 * %%
 * Copyright (C) 2013 - 2020 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import static org.ff4j.redis.RedisContants.KEY_FEATURE;
import static org.ff4j.redis.RedisContants.KEY_FEATURE_MAP;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;

import io.lettuce.core.RedisClient;
import io.lettuce.core.api.sync.RedisCommands;

/**
 * Implementing the feature storage methods.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeatureStoreRedisLettuce extends AbstractFeatureStore {

    /** Lettuce client. */ 
    private RedisCommands<String, String> redisCommands;
    
    /** Default constructor (IOC) */
    public FeatureStoreRedisLettuce() {
        this("redis://localhost");
    }
    
    /** Default constructor. */
    public FeatureStoreRedisLettuce(String connectionString) {
        this(RedisClient.create(connectionString));
    }
    
    /**
     * Public void.
     */
    public FeatureStoreRedisLettuce(RedisClient redisClient) {
        this(redisClient.connect().sync());
    }
    
    /**
     * Public void.
     */
    public FeatureStoreRedisLettuce(RedisCommands<String, String> commands) {
        this.redisCommands = commands;
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertParamHasLength(uid, "Feature identifier");
        return 1 == redisCommands.exists(KEY_FEATURE + uid);
    }
    
    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        return FeatureJsonParser.parseFeature(redisCommands.get(KEY_FEATURE + uid));
    }
    
    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        Util.assertNotNull("Feature" , fp);
        if (!exist(fp.getUid())) {
            throw new FeatureNotFoundException(fp.getUid());
        }
        redisCommands.set(KEY_FEATURE + fp.getUid(), fp.toJson());
        redisCommands.persist(KEY_FEATURE + fp.getUid()); 
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
    public void create(Feature fp) {
        Util.assertNotNull("Feature", fp);
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        redisCommands.sadd(KEY_FEATURE_MAP, fp.getUid());
        redisCommands.set(KEY_FEATURE + fp.getUid(), fp.toJson());
        redisCommands.persist(fp.getUid());
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Set<String> features = redisCommands.smembers(KEY_FEATURE_MAP);
        Map<String, Feature> featuresMap = new HashMap<>();
        if (features != null) {
            for (String key : features) {
                featuresMap.put(key, read(key));
            }
        }
        return featuresMap;
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String fpId) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        redisCommands.srem(KEY_FEATURE_MAP, fpId);
        redisCommands.del(KEY_FEATURE + fpId);
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
        Map < String, Feature > group = new HashMap<String, Feature>();
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
        Map < String, Feature > group = new HashMap<String, Feature>();
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
        Set < String > groups = new HashSet<String>();
        for (Map.Entry<String,Feature> uid : features.entrySet()) {
            groups.add(uid.getValue().getGroup());
        }
        groups.remove(null);
        return groups;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        Set<String> myKeys = redisCommands.smembers(KEY_FEATURE_MAP);
        for (String key : myKeys) {
            delete(key);
        }
    }

}
