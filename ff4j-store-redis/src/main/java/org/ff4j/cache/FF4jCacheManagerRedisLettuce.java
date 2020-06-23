package org.ff4j.cache;

import static org.ff4j.redis.RedisContants.DEFAULT_TTL;
import static org.ff4j.redis.RedisContants.KEY_FEATURE;
import static org.ff4j.redis.RedisContants.KEY_PROPERTY;

import java.util.HashSet;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.utils.json.PropertyJsonParser;

import io.lettuce.core.RedisClient;
import io.lettuce.core.ScanArgs;
import io.lettuce.core.api.sync.RedisCommands;

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

/**
 * Implementation of ditributed cache to limit overhead, with REDIS (JEDIS).
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jCacheManagerRedisLettuce implements FF4JCacheManager {

    /** time to live for cache on top of store. */
    protected int timeToLive = DEFAULT_TTL;
    
    /** Lettuce client. */ 
    private RedisCommands<String, String> redisCommands;
    
    /** Default constructor (IOC) */
    public FF4jCacheManagerRedisLettuce() {
        this("redis://localhost");
    }
    
    /** Default constructor. */
    public FF4jCacheManagerRedisLettuce(String connectionString) {
        this(RedisClient.create(connectionString));
    }
    
    /**
     * Public void.
     */
    public FF4jCacheManagerRedisLettuce(RedisClient redisClient) {
        this(redisClient.connect().sync());
    }
    
    /**
     * Public void.
     */
    public FF4jCacheManagerRedisLettuce(RedisCommands<String, String> commands) {
        this.redisCommands = commands;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedFeatureNames() {
        return getKeys(KEY_FEATURE + "*");
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() {
        return "REDIS";
    }

    /** {@inheritDoc} */
    @Override
    public void clearFeatures() {
        Set<String> matchingKeys = getKeys(KEY_FEATURE + "*");
        if (!matchingKeys.isEmpty()) {
            redisCommands.del(matchingKeys.toArray(new String[matchingKeys.size()]));
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void clearProperties() {
        redisCommands.del(KEY_PROPERTY + "*");
    }

    /** {@inheritDoc} */
    @Override
    public void evictFeature(String uid) {
        Util.assertParamHasLength(uid, " feature identifier");
        redisCommands.del(KEY_FEATURE + uid);
    }

    /** {@inheritDoc} */
    @Override
    public void evictProperty(String propertyName) {
        Util.assertParamHasLength(propertyName, " property name");
        if (propertyName == null || propertyName.isEmpty()) {
            throw new IllegalArgumentException("PropertyName cannot be null nor empty");
        }
        redisCommands.del(KEY_PROPERTY + propertyName);
    }

    /** {@inheritDoc} */
    @Override
    public void putFeature(Feature fp) {
        Util.assertNotNull(fp);
        redisCommands.set(KEY_FEATURE + fp.getUid(), fp.toJson());
        redisCommands.expire(KEY_FEATURE + fp.getUid(), getTimeToLive());
    }

    /** {@inheritDoc} */
    @Override
    public void putProperty(Property<?> property) {
        Util.assertNotNull(property);
        redisCommands.set(KEY_PROPERTY + property.getName(), property.toJson());
        redisCommands.expire(KEY_PROPERTY + property.getName(), getTimeToLive());
    }

    /** {@inheritDoc} */
    @Override
    public Feature getFeature(String uid) {
        Util.assertParamHasLength(uid, "feature uid");
        String value = redisCommands.get(KEY_FEATURE + uid);
        if (value != null) {
            return FeatureJsonParser.parseFeature(value);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> getProperty(String propertyName) {
        Util.assertParamHasLength(propertyName, "property name");
        String value = redisCommands.get(KEY_PROPERTY + propertyName);
        if (value != null) {
            return PropertyJsonParser.parseProperty(value);
        }
        return null;
    }

    /** {@inheritDoc} */
    public Set<String> listCachedPropertyNames() {
        return new HashSet<String>(redisCommands.keys(KEY_PROPERTY + "*"));
    }

    /** {@inheritDoc} */
    public Object getFeatureNativeCache() {
        return redisCommands;
    }

    /** {@inheritDoc} */
    public Object getPropertyNativeCache() {
        return redisCommands;
    }
    
    private Set<String> getKeys(String pattern) {
        return new HashSet<String>(
                redisCommands.scan(new ScanArgs().match(pattern)).getKeys());
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
