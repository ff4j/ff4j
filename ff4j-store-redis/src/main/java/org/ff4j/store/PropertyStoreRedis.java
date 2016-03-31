package org.ff4j.store;

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

import org.ff4j.exception.FeatureAlreadyExistException;
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

    /** prefix of keys. */
    public static final String KEY_PROPERTY = "FF4J_PROPERTY_";
    
    /** default ttl. */
    private static int DEFAULT_TTL = 900000000;
    
    /** time to live. */
    protected int timeToLive = DEFAULT_TTL;
    
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
        Util.assertParamNotNull(name, "PropertyName identifier");
        return getJedis().exists(KEY_PROPERTY + name);
    }

    /** {@inheritDoc} */
    public <T> void createProperty(Property<T> prop) {
        Util.assertNotNull(prop, prop.getName());
        if (existProperty(prop.getName())) {
            throw new FeatureAlreadyExistException( prop.getName());
        }
        getJedis().set(KEY_PROPERTY + prop.getName(), prop.toJson());
        getJedis().persist(KEY_PROPERTY + prop.getName());
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        assertPropertyName(name);
        return PropertyJsonParser.parseProperty(getJedis().get(KEY_PROPERTY + name));
    }  

    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        assertPropertyName(name);
        getJedis().del(KEY_PROPERTY + name);
    }

    /** {@inheritDoc} */
    public Map<String, Property<?>> readAllProperties() {
        LinkedHashMap<String, Property<?>> mapP = new LinkedHashMap<String, Property<?>>();
        Set < String > myKeys = getJedis().keys(KEY_PROPERTY + "*");
        if (myKeys != null) {
            for (String key : myKeys) {
                key = key.replaceAll(KEY_PROPERTY, "");
                mapP.put(key, readProperty(key));
            }
        }
        return mapP;
    }

    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
        return getJedis().keys(KEY_PROPERTY + "*");
    }

    /** {@inheritDoc} */
    public void clear() {
        Set < String > myKeys = getJedis().keys(KEY_PROPERTY + "*");
        getJedis().del(myKeys.toArray(new String[0]));
    }
    
    /**
     * Getter accessor for attribute 'timeToLive'.
     *
     * @return
     *       current value of 'timeToLive'
     */
    public int getTimeToLive() {
        return timeToLive;
    }

    /**
     * Setter accessor for attribute 'timeToLive'.
     * @param timeToLive
     *      new value for 'timeToLive '
     */
    public void setTimeToLive(int timeToLive) {
        this.timeToLive = timeToLive;
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
        if (redisConnection.getJedis() == null) {
            throw new IllegalArgumentException("Cannot found any jedis connection, please build connection");
        }
        return redisConnection.getJedis() ;
    }


}
