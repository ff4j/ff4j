package org.ff4j.store;

import static org.ff4j.redis.RedisContants.KEY_PROPERTY;
import static org.ff4j.redis.RedisContants.KEY_PROPERTY_MAP;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.PropertyJsonParser;

import io.lettuce.core.RedisClient;
import io.lettuce.core.api.sync.RedisCommands;
import io.lettuce.core.cluster.RedisClusterClient;
import io.lettuce.core.cluster.api.sync.RedisAdvancedClusterCommands;

/*
 * #%L
 * ff4j-store-redis
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

/**
 * Implementation of property store for REDIS.
 *
 * @author Cedrick Lunven (@clunven)</a>
 * @author Shridhar Navanageri
 */
public class PropertyStoreRedisLettuce extends AbstractPropertyStore {

    /** Supports sentinel based redis deployment. */ 
    private RedisCommands<String, String> redisCommands;
    
    /** Support the cluster based redis deployment. */
    private RedisAdvancedClusterCommands<String, String> redisCommandsCluster;
    
    /**
     * Public void.
     */
    public PropertyStoreRedisLettuce(RedisClient redisClient) {
        this.redisCommands = redisClient.connect().sync();
    }
    
    /**
     * Using cluster based redis.
     *
     * @param redisClient
     */
    public PropertyStoreRedisLettuce(RedisClusterClient redisClusterClient) {
        this.redisCommandsCluster = redisClusterClient.connect().sync();
    }

    /**
     * {@inheritDoc}
     */
    public boolean existProperty(String name) {
        Util.assertParamHasLength(name, "PropertyName identifier");
        String key   = KEY_PROPERTY + name;
        return 1 == ((null != redisCommands) ? 
                redisCommands.exists(key) : 
                redisCommandsCluster.exists(key));
    }

    /**
     * {@inheritDoc}
     */
    public <T> void createProperty(Property<T> prop) {
        Util.assertNotNull(prop);
        if (existProperty(prop.getName())) {
            throw new PropertyAlreadyExistException(prop.getName());
        }
        if (null != redisCommands) {
            redisCommands.sadd(KEY_PROPERTY_MAP, prop.getName());
            redisCommands.set(KEY_PROPERTY + prop.getName(), prop.toJson());
            redisCommands.persist(KEY_PROPERTY + prop.getName());
        } else {
            redisCommandsCluster.sadd(KEY_PROPERTY_MAP, prop.getName());
            redisCommandsCluster.set(KEY_PROPERTY + prop.getName(), prop.toJson());
            redisCommandsCluster.persist(KEY_PROPERTY + prop.getName());
        }
    }

    /**
     * {@inheritDoc}
     */
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        String key = KEY_PROPERTY + name;
        return PropertyJsonParser.parseProperty((null != redisCommands) ? 
                redisCommands.get(key) : 
                redisCommandsCluster.get(key));
    }

    /**
     * {@inheritDoc}
     */
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        if (null != redisCommands) {
            redisCommands.srem(KEY_PROPERTY_MAP, name);
            redisCommands.del(KEY_PROPERTY + name);
        } else {
            redisCommandsCluster.srem(KEY_PROPERTY_MAP, name);
            redisCommandsCluster.del(KEY_PROPERTY + name);
        }
    }    

    /**
     * {@inheritDoc}
     */
    public Set<String> listPropertyNames() {
        return (null != redisCommands) ? 
                redisCommands.smembers(KEY_PROPERTY_MAP) : 
                redisCommandsCluster.smembers(KEY_PROPERTY_MAP);
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Property<?>> readAllProperties() {
        LinkedHashMap<String, Property<?>> mapP = new LinkedHashMap<String, Property<?>>();
        Set<String> properties = listPropertyNames();
        if (properties != null) {
            for (String key : properties) {
                mapP.put(key, readProperty(key));
            }
         }
         return mapP;
    }
    
    /**
     * {@inheritDoc}
     */
    public void clear() {
        Set<String> myKeys = listPropertyNames();
        for (String key : myKeys) {
            deleteProperty(key);
        }
    }
}
