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
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.utils.json.PropertyJsonParser;

import redis.clients.jedis.Jedis;

/**
 * Implementation of ditributed cache to limit overhead, with REDIS (JEDIS).
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureCacheProviderRedis implements FF4JCacheManager {
    
    /** prefix of keys. */
    public static final String KEY_FEATURE = "FF4J_FEATURE_";
    
    /** prefix of keys. */
    public static final String KEY_PROPERTY = "FF4J_PROPERTY_";
    public static final String FEATURE_IDENTIFIER_PARAM_0_CANNOT_BE_NULL_NOR_EMPTY = "Feature identifier (param#0) cannot be null nor empty";

    /** default ttl. */
    private static int DEFAULT_TTL = 900000000;
    
    /** time to live. */
    protected int timeToLive = DEFAULT_TTL;
    
    /** Wrapping of redis connection (isolation). */
    private RedisConnection redisConnection;
    
    public FeatureCacheProviderRedis() {
        redisConnection = new RedisConnection();
    }

    public FeatureCacheProviderRedis(String host, int port) {
        redisConnection = new RedisConnection(host, port);
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedFeatureNames() {
        return getJedis().keys(KEY_FEATURE + "*");
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() {
        return "REDIS";
    }
    
    /** {@inheritDoc} */
    @Override
    public void clearFeatures() {
        getJedis().del(KEY_FEATURE + "*");
    }

    /** {@inheritDoc} */
    @Override
    public void clearProperties() {
        getJedis().del(KEY_PROPERTY + "*");
    }

    /** {@inheritDoc} */
    @Override
    public void evictFeature(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_PARAM_0_CANNOT_BE_NULL_NOR_EMPTY);
        }
        getJedis().del(KEY_FEATURE + uid);
    }

    /** {@inheritDoc} */
    @Override
    public void evictProperty(String propertyName) {
        if (propertyName == null || propertyName.isEmpty()) {
            throw new IllegalArgumentException("PropertyName cannot be null nor empty");
        }
        getJedis().del(KEY_PROPERTY + propertyName);
    }

    @Override
    public void putFeature(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        getJedis().set(KEY_FEATURE + fp.getUid(), fp.toJson());
        getJedis().expire(KEY_FEATURE + fp.getUid(), timeToLive);
    }

    @Override
    public void putProperty(Property<?> property) {
        if (property == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        getJedis().set(KEY_PROPERTY + property.getName(), property.toJson());
        getJedis().expire(KEY_PROPERTY + property.getName(), timeToLive);
    }

    /** {@inheritDoc} */
    @Override
    public Feature getFeature(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_PARAM_0_CANNOT_BE_NULL_NOR_EMPTY);
        }
        String value = getJedis().get(KEY_FEATURE + uid);
        if (value != null) {
            return FeatureJsonParser.parseFeature(value);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> getProperty(String propertyName) {
        if (propertyName == null || propertyName.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_PARAM_0_CANNOT_BE_NULL_NOR_EMPTY);
        }
        String value = getJedis().get(KEY_PROPERTY + propertyName);
        if (value != null) {
            return PropertyJsonParser.parseProperty(value);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedPropertyNames() {
        return getJedis().keys(KEY_PROPERTY + "*");
    }

    /** {@inheritDoc} */
    @Override
    public Object getFeatureNativeCache() {
        return getJedis();
    }

    /** {@inheritDoc} */
    @Override
    public Object getPropertyNativeCache() {
        return getJedis();
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

}
