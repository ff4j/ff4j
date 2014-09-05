package org.ff4j.store;

/*
 * #%L
 * ff4j-store-redis
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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.redis.AbstractRedisProvider;
import org.ff4j.utils.json.FeatureJsonParser;

import redis.clients.jedis.Jedis;

/**
 * {@link FeatureStore} to persist data into
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreRedis extends AbstractRedisProvider implements FeatureStore {
    
    /**
     * Default Constructor.
     */
    public FeatureStoreRedis() {
        jedis = new Jedis(redisHost, redisport);
    }

    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public FeatureStoreRedis(String host, int port) {
        jedis = new Jedis(redisHost, redisport);
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        return jedis.exists(PREFIX_KEY + uid);
    }
    
    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        return FeatureJsonParser.parseFeature(jedis.get(PREFIX_KEY + uid));
    }
    
    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        if (!exist(fp.getUid())) {
            throw new FeatureNotFoundException(fp.getUid());
        }
        jedis.set(PREFIX_KEY + fp.getUid(), fp.toJson());
        jedis.persist(PREFIX_KEY + fp.getUid());
    }
    
    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        Feature f = read(uid);
        f.enable();
        update(f);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        Feature f = read(uid);
        f.disable();
        update(f);
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        update(fp);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Set < String > myKeys = jedis.keys(PREFIX_KEY + "*");
        Map<String, Feature> myMap = new HashMap<String, Feature>();
        if (myKeys != null) {
            for (String key : myKeys) {
                myMap.put(key, read(key));
            }
        }
        return myMap;
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String fpId) {
    }    

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void removeFromGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Set<String> readAllGroups() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean isCached() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public String getCacheProvider() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getCachedTargetStore() {
        // TODO Auto-generated method stub
        return null;
    }
    

}
