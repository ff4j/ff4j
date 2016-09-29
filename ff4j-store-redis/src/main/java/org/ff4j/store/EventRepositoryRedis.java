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


import static org.ff4j.redis.RedisContants.KEY_EVENT;

import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.redis.RedisConnection;

import redis.clients.jedis.Jedis;

/**
 * Persist audit events into REDIS storage technology.
 *
 * @author clunven
 */
public class EventRepositoryRedis extends AbstractEventRepository {
	
    /** Wrapping of redis connection (isolation). */
    private RedisConnection redisConnection;
    
	/**
     * Default Constructor.
     */
    public EventRepositoryRedis() {
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
    public EventRepositoryRedis(RedisConnection pRedisConnection) {
        redisConnection = pRedisConnection;
    }

    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public EventRepositoryRedis(String host, int port) {
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
    public EventRepositoryRedis(String host, int port, String password) {
        this(new RedisConnection(host, port, password));
    }
    
	/** {@inheritDoc} */
	public boolean saveEvent(Event evt) {
	    // The key will be : FF4J_EVENTS_FEAUID_YYYYMMDD to be able to group and search
	    // The key will be : FF4J_EVENTS_AUDITTRAIL_YYYYMMDD to be able to group and search
        
	    
	    // Then, the value will be a sorted SET and algortihms will look like InMemory
		 if (evt == null) {
			 throw new IllegalArgumentException("Event cannot be null nor empty");
	     }
		 Jedis jedis = null;
	     try {
	         jedis = getJedis();
    		 String uid = KEY_EVENT + "-" + evt.getTimestamp() + "-" + evt.getUuid();
    	     jedis.set(uid, evt.toJson());
    	     jedis.persist(uid);
    		 return true;
	        } finally {
	            if (jedis != null) {
	                jedis.close();
	            }
	        }
	}
    
    /**
     * Safe acces to Jedis, avoid JNPE.
     *
     * @return
     *      access jedis
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

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        // There is no schema to create, it's key value (flat) and keys are create FF4J_FEATURE_${FEATUREID}
        return;
    }

    /** {@inheritDoc} */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit tu) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        
    }
}