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

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import io.lettuce.core.api.sync.RedisKeyCommands;
import io.lettuce.core.api.sync.RedisSetCommands;
import io.lettuce.core.api.sync.RedisStringCommands;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.redis.RedisKeysBuilder;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.PropertyJsonParser;

import io.lettuce.core.RedisClient;
import io.lettuce.core.api.sync.RedisCommands;
import io.lettuce.core.cluster.RedisClusterClient;
import io.lettuce.core.cluster.api.sync.RedisAdvancedClusterCommands;

/**
 * Implementation of property store for REDIS.
 *
 * @author Cedrick Lunven (@clunven)</a>
 * @author Shridhar Navanageri
 */
public class PropertyStoreRedisLettuce extends AbstractPropertyStore {

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
    public PropertyStoreRedisLettuce(RedisClient redisClient) {
        this(redisClient, new RedisKeysBuilder());
    }
    public PropertyStoreRedisLettuce(RedisClient redisClient, RedisKeysBuilder keyBuilder) {
        this.redisKeyCommands = redisClient.connect().sync();
        this.redisStringCommands = redisClient.connect().sync();
        this.redisSetCommands = redisClient.connect().sync();
        this.keyBuilder    = keyBuilder;
    }
    public PropertyStoreRedisLettuce(RedisClusterClient redisClusterClient) {
        this(redisClusterClient, new RedisKeysBuilder());
    }
    public PropertyStoreRedisLettuce(RedisClusterClient redisClusterClient, RedisKeysBuilder keyBuilder) {
        this.redisKeyCommands = redisClusterClient.connect().sync();
        this.redisStringCommands = redisClusterClient.connect().sync();
        this.redisSetCommands = redisClusterClient.connect().sync();
        this.keyBuilder    = keyBuilder;
    }

    /**
     * {@inheritDoc}
     */
    public boolean existProperty(String name) {
        Util.assertParamHasLength(name, "PropertyName identifier");
        String key   = keyBuilder.getKeyProperty(name);
        return 1 == redisKeyCommands.exists(key);
    }

    /**
     * {@inheritDoc}
     */
    public <T> void createProperty(Property<T> prop) {
        Util.assertNotNull(prop);
        if (existProperty(prop.getName())) {
            throw new PropertyAlreadyExistException(prop.getName());
        }
        redisSetCommands.sadd(keyBuilder.getKeyPropertyMap(), prop.getName());
        redisStringCommands.set(keyBuilder.getKeyProperty(prop.getName()), prop.toJson());
        redisKeyCommands.persist(keyBuilder.getKeyProperty(prop.getName()));
    }

    /**
     * {@inheritDoc}
     */
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        String key = keyBuilder.getKeyProperty(name);
        return PropertyJsonParser.parseProperty(redisStringCommands.get(key));
    }

    /**
     * {@inheritDoc}
     */
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        redisSetCommands.srem(keyBuilder.getKeyPropertyMap(), name);
        redisKeyCommands.del(keyBuilder.getKeyProperty(name));
    }    

    /**
     * {@inheritDoc}
     */
    public Set<String> listPropertyNames() {
        return redisSetCommands.smembers(keyBuilder.getKeyPropertyMap());
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
