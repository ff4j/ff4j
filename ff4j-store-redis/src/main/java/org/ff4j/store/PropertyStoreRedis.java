package org.ff4j.store;

import static org.ff4j.redis.RedisContants.KEY_PROPERTY;

import java.util.HashSet;

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


import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.redis.RedisConnection;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.PropertyJsonParser;

import redis.clients.jedis.Jedis;

/**
 * Implementation of property store for REDIS.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreRedis extends AbstractPropertyStore {
    
    /** Wrapping of redis connection (isolation). */
    private RedisConnection redisConnection;
    
    /**
     * Default Constructor.
     */
    public PropertyStoreRedis() {
        this(new RedisConnection());
    }
    
    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public PropertyStoreRedis(RedisConnection pRedisConnection) {
        redisConnection = pRedisConnection;
    }
    
    /**
     * Default Constructor.
     */
    public PropertyStoreRedis(String xmlFeaturesfFile) {
       this();
       importPropertiesFromXmlFile(xmlFeaturesfFile);
    }

    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public PropertyStoreRedis(String host, int port) {
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
    public PropertyStoreRedis(String host, int port, String password, String xmlFeaturesfFile) {
        this(new RedisConnection(host, port, password));
        importPropertiesFromXmlFile(xmlFeaturesfFile);
    }

    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public PropertyStoreRedis(String host, int port, String xmlFeaturesfFile) {
        this(host, port);
        importPropertiesFromXmlFile(xmlFeaturesfFile);
    }
    
    /** {@inheritDoc} */
    public boolean existProperty(String name) {
        Util.assertParamHasLength(name, "PropertyName identifier");
        Jedis jedis = null;
        try {
            jedis = getJedis();
            return jedis.exists(KEY_PROPERTY + name);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        } 
    }

    /** {@inheritDoc} */
    public <T> void createProperty(Property<T> prop) {
        Util.assertNotNull(prop);
        if (existProperty(prop.getName())) {
            throw new PropertyAlreadyExistException(prop.getName());
        }
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.set(KEY_PROPERTY + prop.getName(), prop.toJson());
            jedis.persist(KEY_PROPERTY + prop.getName());
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        } 
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        Jedis jedis = null;
        try {
            jedis = getJedis();
            return PropertyJsonParser.parseProperty(jedis.get(KEY_PROPERTY + name));
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        } 
    }  

    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.del(KEY_PROPERTY + name);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        } 
    }

    /** {@inheritDoc} */
    public Map<String, Property<?>> readAllProperties() {
        LinkedHashMap<String, Property<?>> mapP = new LinkedHashMap<String, Property<?>>();
        Jedis jedis = null;
        try {
            jedis = getJedis();
            Set < String > myKeys = jedis.keys(KEY_PROPERTY + "*");
            if (myKeys != null) {
                for (String key : myKeys) {
                    key = key.replaceAll(KEY_PROPERTY, "");
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

    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
        Jedis jedis = null;
        try {
            jedis = getJedis();
            Set < String > propertyNames = jedis.keys(KEY_PROPERTY + "*");
            Set < String > responses = new HashSet<String>();
            if (propertyNames != null) {
                for (String pName : propertyNames) {
                    responses.add(pName.replaceAll(KEY_PROPERTY, ""));
                }
            }
            return responses;
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        } 
    }

    /** {@inheritDoc} */
    public void clear() {
        Jedis jedis = null;
        try {
            jedis = getJedis();
            Set < String > myKeys = jedis.keys(KEY_PROPERTY + "*");
            jedis.del(myKeys.toArray(new String[0]));
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
     *      new value for 'redisConnection '
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
