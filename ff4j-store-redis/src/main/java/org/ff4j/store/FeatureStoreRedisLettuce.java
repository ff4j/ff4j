package org.ff4j.store;

/*-
 * #%L
 * ff4j-store-redis
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import io.lettuce.core.api.sync.RedisKeyCommands;
import io.lettuce.core.api.sync.RedisSetCommands;
import io.lettuce.core.api.sync.RedisStringCommands;
import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.redis.RedisKeysBuilder;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;

import io.lettuce.core.RedisClient;
import io.lettuce.core.api.sync.RedisCommands;
import io.lettuce.core.cluster.RedisClusterClient;
import io.lettuce.core.cluster.api.sync.RedisAdvancedClusterCommands;

/**
 * Implementing the feature storage methods.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeatureStoreRedisLettuce extends AbstractFeatureStore {

    /** Access to Redis Key related commands */
    private final RedisKeyCommands<String, String> redisKeyCommands;

    /** Access to Redis String based commands */
    private final RedisStringCommands<String, String> redisStringCommands;

    /** Access to Redis Set based commands */
    private final RedisSetCommands<String, String> redisSetCommands;

    /** Default key builder. */
    private final RedisKeysBuilder keyBuilder;

    /**
     * Public void.
     */
    public FeatureStoreRedisLettuce(RedisClient redisClient) {
        this(redisClient, new RedisKeysBuilder());
    }
    public FeatureStoreRedisLettuce(RedisClient redisClient, RedisKeysBuilder keyBuilder) {
        this.redisKeyCommands = redisClient.connect().sync();
        this.redisStringCommands = redisClient.connect().sync();
        this.redisSetCommands = redisClient.connect().sync();
        this.keyBuilder    = keyBuilder;
    }
    public FeatureStoreRedisLettuce(RedisClusterClient redisClusterClient) {
        this(redisClusterClient, new RedisKeysBuilder());
    }
    public FeatureStoreRedisLettuce(RedisClusterClient redisClusterClient, RedisKeysBuilder keyBuilder) {
        this.redisKeyCommands = redisClusterClient.connect().sync();
        this.redisStringCommands = redisClusterClient.connect().sync();
        this.redisSetCommands = redisClusterClient.connect().sync();
        this.keyBuilder    = keyBuilder;
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertParamHasLength(uid, "Feature identifier");
        String key = keyBuilder.getKeyFeature(uid);
        return 1 == redisKeyCommands.exists(key);
    }
    
    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        String key = keyBuilder.getKeyFeature(uid);
        return FeatureJsonParser.parseFeature(redisStringCommands.get(key));
    }
    
    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        Util.assertNotNull("Feature" , fp);
        if (!exist(fp.getUid())) {
            throw new FeatureNotFoundException(fp.getUid());
        }
        String key = keyBuilder.getKeyFeature(fp.getUid());
        redisStringCommands.set(key, fp.toJson());
        redisKeyCommands.persist(key);
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
        redisSetCommands.sadd(keyBuilder.getKeyFeatureMap(), fp.getUid());
        redisStringCommands.set(keyBuilder.getKeyFeature(fp.getUid()), fp.toJson());
        redisKeyCommands.persist(fp.getUid());
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Set<String> features = redisSetCommands.smembers(keyBuilder.getKeyFeatureMap());
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
        redisSetCommands.srem(keyBuilder.getKeyFeatureMap(), fpId);
        redisKeyCommands.del(keyBuilder.getKeyFeature(fpId));
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
        Set<String> myKeys = redisSetCommands.smembers(keyBuilder.getKeyFeatureMap());
        for (String key : myKeys) {
            delete(key);
        }
    }

}
