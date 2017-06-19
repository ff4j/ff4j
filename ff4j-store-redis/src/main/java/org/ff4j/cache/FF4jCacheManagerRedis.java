package org.ff4j.cache;

/*
 * #%L
 * ff4j-cache-redis
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

import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.redis.RedisConnection;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.utils.json.PropertyJsonParser;

import redis.clients.jedis.Jedis;
import static org.ff4j.redis.RedisContants.*;

/**
 * Implementation of ditributed cache to limit overhead, with REDIS (JEDIS).
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jCacheManagerRedis implements FF4JCacheManager {
    
    /** Wrapping of redis connection (isolation). */
    private RedisConnection redisConnection;
    
    /** time to live for cache on top of store. */
    protected int timeToLive = DEFAULT_TTL;
    
    /**
     * Default constructor
     */
    public FF4jCacheManagerRedis(RedisConnection redisConn) {
        redisConnection = redisConn;
    }
    
    /**
     * Default constructor
     */
    public FF4jCacheManagerRedis() {
        this(new RedisConnection());
    }

    public FF4jCacheManagerRedis(String host, int port) {
        redisConnection = new RedisConnection(host, port);
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedFeatureNames() {
        Jedis jedis = null;
        try {
            jedis = getJedis();
            return jedis.keys(KEY_FEATURE + "*");
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() {
        return "REDIS";
    }
    
    /** {@inheritDoc} */
    @Override
    public void clearFeatures() {
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.del(KEY_FEATURE + "*");
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void clearProperties() {
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.del(KEY_PROPERTY + "*");
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void evictFeature(String uid) {
        Util.assertParamHasLength(uid, " feature identifier");
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.del(KEY_FEATURE + uid);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void evictProperty(String propertyName) {
        Util.assertParamHasLength(propertyName, " property name");
        if (propertyName == null || propertyName.isEmpty()) {
            throw new IllegalArgumentException("PropertyName cannot be null nor empty");
        }
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.del(KEY_PROPERTY + propertyName);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        } 
    }

    /** {@inheritDoc} */
    @Override
    public void putFeature(Feature fp) {
        Util.assertNotNull(fp);
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.set(KEY_FEATURE + fp.getUid(), fp.toJson());
            jedis.expire(KEY_FEATURE + fp.getUid(), getTimeToLive());
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        } 
    }

    /** {@inheritDoc} */
    @Override
    public void putProperty(Property<?> property) {
        Util.assertNotNull(property);
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.set(KEY_PROPERTY + property.getName(), property.toJson());
            jedis.expire(KEY_PROPERTY + property.getName(), getTimeToLive());
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        } 
    }

    /** {@inheritDoc} */
    @Override
    public Feature getFeature(String uid) {
        Util.assertParamHasLength(uid, "feature uid");
        Jedis jedis = null;
        try {
            jedis = getJedis();
            String value = jedis.get(KEY_FEATURE + uid);
            if (value != null) {
                return FeatureJsonParser.parseFeature(value);
            }
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        } 
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> getProperty(String propertyName) {
        Util.assertParamHasLength(propertyName, "property name");
        Jedis jedis = null;
        try {
            jedis = getJedis();
            String value = jedis.get(KEY_PROPERTY + propertyName);
            if (value != null) {
                return PropertyJsonParser.parseProperty(value);
            }
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }   
        return null;
    }

    /** {@inheritDoc} */
    public Set<String> listCachedPropertyNames() {
        Jedis jedis = null;
        try {
            jedis = getJedis();
            return jedis.keys(KEY_PROPERTY + "*");
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    public Object getFeatureNativeCache() {
        return getJedis();
    }

    /** {@inheritDoc} */
    public Object getPropertyNativeCache() {
        return getJedis();
    } 
    
    /**
     * Safe acces to Jedis, avoid JNPE.
     *
     * @return
     */
    public Jedis getJedis() {
        Util.assertNotNull(redisConnection);
        Jedis jedis = redisConnection.getJedis();
        if (jedis == null) {
            throw new IllegalArgumentException("Cannot found any jedis connection, please build connection");
        }
        return jedis;
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
     *
     * @param timeToLive
     *      new value for 'timeToLive '
     */
    public void setTimeToLive(int timeToLive) {
        this.timeToLive = timeToLive;
    }

}
