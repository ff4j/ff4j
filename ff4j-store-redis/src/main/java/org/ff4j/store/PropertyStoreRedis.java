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

import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.redis.RedisConnection;
import org.ff4j.redis.RedisKeysBuilder;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.PropertyJsonParser;

import redis.clients.jedis.Jedis;

/**
 * Implementation of property store for REDIS.
 *
 * @author Cedrick Lunven (@clunven)</a>
 * @author Shridhar Navanageri
 */
public class PropertyStoreRedis extends AbstractPropertyStore {

    /**
     * Wrapping of redis connection (isolation).
     */
    private RedisConnection redisConnection;
    
    /** Default key builder. */
    private RedisKeysBuilder keyBuilder = new RedisKeysBuilder();

    public PropertyStoreRedis() {
        this(new RedisConnection(), new RedisKeysBuilder());
    }
    public PropertyStoreRedis(RedisKeysBuilder builder) {
        this(new RedisConnection(), builder);
    }
    public PropertyStoreRedis(RedisConnection pRedisConnection) {
        this(pRedisConnection, new RedisKeysBuilder());
    }
    public PropertyStoreRedis(RedisConnection pRedisConnection, RedisKeysBuilder builder) {
        this.redisConnection = pRedisConnection;
        this.keyBuilder      = builder;
    }
    
    /**
     * {@inheritDoc}
     */
    public boolean existProperty(String name) {
        Util.assertParamHasLength(name, "PropertyName identifier");
        Jedis jedis = null;
        try {
            jedis = getJedis();
            return jedis.exists(keyBuilder.getKeyProperty(name));
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public <T> void createProperty(Property<T> prop) {
        Util.assertNotNull(prop);
        if (existProperty(prop.getName())) {
            throw new PropertyAlreadyExistException(prop.getName());
        }
        Jedis jedis = null;
        try {
            jedis = getJedis();
            String name = prop.getName();
            // Store the feature in the mapping bucket.
            jedis.sadd(keyBuilder.getKeyPropertyMap(), name);
            jedis.set(keyBuilder.getKeyProperty(name), prop.toJson());
            jedis.persist(keyBuilder.getKeyProperty(name));
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        Jedis jedis = null;
        try {
            jedis = getJedis();
            return PropertyJsonParser.parseProperty(
                    jedis.get(keyBuilder.getKeyProperty(name)));
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.srem(keyBuilder.getKeyPropertyMap(), name);
            jedis.del(keyBuilder.getKeyProperty(name));
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public Map<String, Property<?>> readAllProperties() {
        LinkedHashMap<String, Property<?>> mapP = new LinkedHashMap<String, Property<?>>();
        Jedis jedis = null;
        try {
            jedis = getJedis();
            Set<String> properties = jedis.smembers(keyBuilder.getKeyPropertyMap());
            if (properties != null) {
                for (String key : properties) {
                    mapP.put(key, readProperty(key));
                }
            }
            return mapP;
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public Set<String> listPropertyNames() {
        Jedis jedis = null;
        try {
            jedis = getJedis();
            Set<String> propertyNames = jedis.smembers(keyBuilder.getKeyPropertyMap());
            return propertyNames;
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public void clear() {
        Jedis jedis = null;
        try {
            jedis = getJedis();
            Set<String> myKeys = jedis.smembers(keyBuilder.getKeyPropertyMap());
            for (String key : myKeys) {
                deleteProperty(key);
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
     * @return current value of 'redisConnection'
     */
    public RedisConnection getRedisConnection() {
        return redisConnection;
    }

    /**
     * Setter accessor for attribute 'redisConnection'.
     *
     * @param redisConnection new value for 'redisConnection '
     */
    public void setRedisConnection(RedisConnection redisConnection) {
        this.redisConnection = redisConnection;
    }

    /**
     * Safe acces to Jedis, avoid JNPE.
     *
     * @return
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
