package org.ff4j.store;

/*
 * #%L
 * ff4j-store-redis
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.redis.RedisConnection;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;
import redis.clients.jedis.Jedis;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.ff4j.redis.RedisContants.KEY_FEATURE;
import static org.ff4j.redis.RedisContants.KEY_FEATURE_MAP;

/**
 * {@link FeatureStore} to persist data into
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 * @author Shridhar Navanageri
 */
public class FeatureStoreRedis extends AbstractFeatureStore {
    
    /** Wrapping of redis connection (isolation). */
    private RedisConnection redisConnection;
    
    /**
     * Default Constructor.
     */
    public FeatureStoreRedis() {
        this(new RedisConnection());
    }
    
    /**
     * Contact remote redis server.
     *
     */
    public FeatureStoreRedis(RedisConnection pRedisConnection) {
        redisConnection = pRedisConnection;
    }
    
    /**
     * Default Constructor.
     */
    public FeatureStoreRedis(String xmlFeaturesfFile) {
       this();
       importFeaturesFromXmlFile(xmlFeaturesfFile);
    }

    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public FeatureStoreRedis(String host, int port) {
        this(new RedisConnection(host, port));
    }
    
    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public FeatureStoreRedis(String host, int port, String password, String xmlFeaturesfFile) {
        this(new RedisConnection(host, port, password));
        importFeaturesFromXmlFile(xmlFeaturesfFile);
    }

    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public FeatureStoreRedis(String host, int port, String xmlFeaturesfFile) {
        this(host, port);
        importFeaturesFromXmlFile(xmlFeaturesfFile);
    }
    
    /** {@inheritDoc} */
    public boolean exist(String uid) {
        Util.assertParamHasLength(uid, "Feature identifier");
        Jedis jedis = null;
        try {
            jedis = getJedis();
            return jedis.exists(KEY_FEATURE + uid);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        Jedis jedis = null;
        try {
            jedis = getJedis();
            return FeatureJsonParser.parseFeature(jedis.get(KEY_FEATURE + uid));
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        Util.assertNotNull("Feature" , fp);
        if (!exist(fp.getUid())) {
            throw new FeatureNotFoundException(fp.getUid());
        }
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.set(KEY_FEATURE + fp.getUid(), fp.toJson());
            jedis.persist(KEY_FEATURE + fp.getUid());
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
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
        Jedis jedis = null;
        try {
            String id = fp.getUid();
            jedis = getJedis();

            // Store the feature in the mapping bucket.
            jedis.sadd(KEY_FEATURE_MAP, id);
            jedis.set(KEY_FEATURE + id, fp.toJson());
            jedis.persist(KEY_FEATURE + id);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Jedis jedis = null;
        try {
            jedis = getJedis();

            Set<String> features = jedis.smembers(KEY_FEATURE_MAP);

            Map<String, Feature> featuresMap = new HashMap<>();
            if (features != null) {
                for (String key : features) {
                    featuresMap.put(key, read(key));
                }
            }
            return featuresMap;
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    public void delete(String fpId) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        Jedis jedis = null;
        try {
            jedis = getJedis();
            // Store the feature in the mapping bucket.
            jedis.srem(KEY_FEATURE_MAP, fpId);
            jedis.del(KEY_FEATURE + fpId);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
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
        Jedis jedis = null;
        try {
            jedis = getJedis();
            Set<String> myKeys = jedis.smembers(KEY_FEATURE_MAP);
            for (String key : myKeys) {
                delete(key);
            }
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /**
     * Getter accessor for attribute 'redisConnection'.
     *
     * @return
     *       current value of 'redisConnection'
     */
    public RedisConnection getRedisConnection() {
        return redisConnection;
    }

    /**
     * Setter accessor for attribute 'redisConnection'.
     * @param redisConnection
     * 		new value for 'redisConnection '
     */
    public void setRedisConnection(RedisConnection redisConnection) {
        this.redisConnection = redisConnection;
    }
    
    /**
     * Safe acces to Jedis, avoid JNPE.
     *
     * @return
     *      access jedis
     */
    public Jedis getJedis() {
        if (redisConnection == null) {
            throw new IllegalArgumentException("Cannot found any redisConnection");
        }
        Jedis jedis = redisConnection.getJedis();
        if (jedis == null) {
            throw new IllegalArgumentException("Cannot found any jedis connection, please build connection");
        }
        return jedis;
    }

}
