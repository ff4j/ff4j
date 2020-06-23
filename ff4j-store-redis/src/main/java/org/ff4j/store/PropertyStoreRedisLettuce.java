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

    /** Lettuce client. */ 
    private RedisCommands<String, String> redisCommands;
    
    /** Default constructor (IOC) */
    public PropertyStoreRedisLettuce() {
        this("redis://localhost");
    }
    
    /** Default constructor. */
    public PropertyStoreRedisLettuce(String connectionString) {
        this(RedisClient.create(connectionString));
    }
    
    /**
     * Public void.
     */
    public PropertyStoreRedisLettuce(RedisClient redisClient) {
        this(redisClient.connect().sync());
    }
    
    /**
     * Public void.
     */
    public PropertyStoreRedisLettuce(RedisCommands<String, String> commands) {
        this.redisCommands = commands;
    }

    /**
     * {@inheritDoc}
     */
    public boolean existProperty(String name) {
        Util.assertParamHasLength(name, "PropertyName identifier");
        return 1 == redisCommands.exists(KEY_PROPERTY + name);
    }

    /**
     * {@inheritDoc}
     */
    public <T> void createProperty(Property<T> prop) {
        Util.assertNotNull(prop);
        if (existProperty(prop.getName())) {
            throw new PropertyAlreadyExistException(prop.getName());
        }
        redisCommands.sadd(KEY_PROPERTY_MAP, prop.getName());
        redisCommands.set(KEY_PROPERTY + prop.getName(), prop.toJson());
        redisCommands.persist(KEY_PROPERTY + prop.getName());
    }

    /**
     * {@inheritDoc}
     */
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        return PropertyJsonParser.parseProperty(redisCommands.get(KEY_PROPERTY + name));
    }

    /**
     * {@inheritDoc}
     */
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        redisCommands.srem(KEY_PROPERTY_MAP, name);
        redisCommands.del(KEY_PROPERTY + name);
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Property<?>> readAllProperties() {
        LinkedHashMap<String, Property<?>> mapP = new LinkedHashMap<String, Property<?>>();
        Set<String> properties = redisCommands.smembers(KEY_PROPERTY_MAP);
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
    public Set<String> listPropertyNames() {
        return redisCommands.smembers(KEY_PROPERTY_MAP);
    }

    /**
     * {@inheritDoc}
     */
    public void clear() {
        Set<String> myKeys = redisCommands.smembers(KEY_PROPERTY_MAP);
        for (String key : myKeys) {
            deleteProperty(key);
        }
    }
}
