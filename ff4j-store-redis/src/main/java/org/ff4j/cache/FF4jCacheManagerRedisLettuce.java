package org.ff4j.cache;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.redis.RedisKeysBuilder;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.utils.json.PropertyJsonParser;

import io.lettuce.core.KeyScanCursor;
import io.lettuce.core.RedisClient;
import io.lettuce.core.ScanArgs;
import io.lettuce.core.api.sync.RedisCommands;
import io.lettuce.core.cluster.RedisClusterClient;
import io.lettuce.core.cluster.api.sync.RedisAdvancedClusterCommands;

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
    protected int timeToLive = 900000000;
    
    /** Lettuce client. */ 
    private RedisCommands<String, String> redisCommands;
    
    /** Support the cluster based redis deployment. */
    private RedisAdvancedClusterCommands<String, String> redisCommandsCluster;
    
    /** Default key builder. */
    private RedisKeysBuilder keyBuilder = new RedisKeysBuilder();
    
    /**
     * Public void.
     */
    public FF4jCacheManagerRedisLettuce(RedisClient redisClient) {
        this(redisClient, new RedisKeysBuilder());
    }
    public FF4jCacheManagerRedisLettuce(RedisClient redisClient, RedisKeysBuilder keyBuilder) {
        this.redisCommands = redisClient.connect().sync();
        this.keyBuilder    = keyBuilder;
    }
    public FF4jCacheManagerRedisLettuce(RedisClusterClient redisClusterClient) {
        this(redisClusterClient, new RedisKeysBuilder());
    }
    public FF4jCacheManagerRedisLettuce(RedisClusterClient redisClusterClient, RedisKeysBuilder keyBuilder) {
        this.redisCommandsCluster = redisClusterClient.connect().sync();
        this.keyBuilder    = keyBuilder;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedFeatureNames() {
        return getKeys(keyBuilder.getKeyFeature("*"));
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() {
        return "REDIS";
    }

    /** {@inheritDoc} */
    @Override
    public void clearFeatures() {
        Set<String> matchingKeys = getKeys(keyBuilder.getKeyFeature("*"));
        if (!matchingKeys.isEmpty()) {
            if (null != redisCommands) {
                redisCommands.del(matchingKeys.toArray(new String[matchingKeys.size()]));
            } else {
                redisCommandsCluster.del(matchingKeys.toArray(new String[matchingKeys.size()]));
            }
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void clearProperties() {
        if (null != redisCommands) {
            redisCommands.del(keyBuilder.getKeyProperty("*"));
        } else {
            redisCommandsCluster.del(keyBuilder.getKeyProperty("*"));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void evictFeature(String uid) {
        Util.assertParamHasLength(uid, " feature identifier");
        if (null != redisCommands) {
            redisCommands.del(keyBuilder.getKeyFeature(uid));
        } else {
            redisCommandsCluster.del(keyBuilder.getKeyFeature(uid));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void evictProperty(String propertyName) {
        Util.assertParamHasLength(propertyName, " property name");
        if (propertyName == null || propertyName.isEmpty()) {
            throw new IllegalArgumentException("PropertyName cannot be null nor empty");
        }
        if (null != redisCommands) {
            redisCommands.del(keyBuilder.getKeyProperty(propertyName));
        } else {
            redisCommandsCluster.del(keyBuilder.getKeyProperty(propertyName));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void putFeature(Feature fp) {
        Util.assertNotNull(fp);
        if (null != redisCommands) { 
            redisCommands.set(keyBuilder.getKeyFeature(fp.getUid()), fp.toJson());
            redisCommands.expire(keyBuilder.getKeyFeature(fp.getUid()), getTimeToLive());
        } else {
            redisCommandsCluster.set(keyBuilder.getKeyFeature(fp.getUid()), fp.toJson());
            redisCommandsCluster.expire(keyBuilder.getKeyFeature(fp.getUid()), getTimeToLive());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void putProperty(Property<?> property) {
        Util.assertNotNull(property);
        if (null != redisCommands) {
            redisCommands.set(keyBuilder.getKeyProperty(property.getName()), property.toJson());
            redisCommands.expire(keyBuilder.getKeyProperty(property.getName()), getTimeToLive());
        } else {
            redisCommandsCluster.set(keyBuilder.getKeyProperty(property.getName()), property.toJson());
            redisCommandsCluster.expire(keyBuilder.getKeyProperty(property.getName()), getTimeToLive());
        }
    }

    /** {@inheritDoc} */
    @Override
    public Feature getFeature(String uid) {
        Util.assertParamHasLength(uid, "feature uid");
        String value = "";
        if (null != redisCommands) {
            value = redisCommands.get(keyBuilder.getKeyFeature(uid));
        } else {
            value = redisCommandsCluster.get(keyBuilder.getKeyFeature(uid));
        }
        if (value != null) {
            return FeatureJsonParser.parseFeature(value);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> getProperty(String propertyName) {
        Util.assertParamHasLength(propertyName, "property name");
        String value = "";
        if (null != redisCommands) {
            value = redisCommands.get(keyBuilder.getKeyProperty(propertyName));
        } else {
            value = redisCommandsCluster.get(keyBuilder.getKeyProperty(propertyName));
        }
        if (value != null) {
            return PropertyJsonParser.parseProperty(value);
        }
        return null;
    }

    /** {@inheritDoc} */
    public Set<String> listCachedPropertyNames() {
        List <String > keys = new ArrayList<>();
        if (null != redisCommands) {
            keys = redisCommands.keys(keyBuilder.getKeyProperty("*"));
        } else {
            keys = redisCommandsCluster.keys(keyBuilder.getKeyProperty("*"));
        }
        return new HashSet<String>(keys);
    }

    /** {@inheritDoc} */
    public Object getFeatureNativeCache() {
        return (null != redisCommands) ? redisCommands : redisCommandsCluster;
    }

    /** {@inheritDoc} */
    public Object getPropertyNativeCache() {
        return (null != redisCommands) ? redisCommands : redisCommandsCluster;
    }
    
    private Set<String> getKeys(String pattern) {
        ScanArgs scan = new ScanArgs().match(pattern);
        KeyScanCursor<String> ksc = null;
        if (null != redisCommands) {
            ksc = redisCommands.scan(scan);
        } else {
            ksc = redisCommandsCluster.scan(scan);
        }
        return new HashSet<String>(ksc.getKeys());
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
    
    /**
     * Getter accessor for attribute 'keyBuilder'.
     *
     * @return
     *       current value of 'keyBuilder'
     */
    public RedisKeysBuilder getKeyBuilder() {
        return keyBuilder;
    }

}
