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

import java.util.Set;

import org.ff4j.audit.Event;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.redis.RedisConnection;

import redis.clients.jedis.Jedis;

/**
 * Persist audit events into REDIS storage technology.
 *
 * @author clunven
 */
public class EventRepositoryRedis extends AbstractEventRepository {

	/** prefix of keys. */
    public static final String KEY_EVENT = "FF4J_EVENT_";
    
    /** Default ttl. */
    private static int DEFAULT_TTL = 900000000;
    
    /** Time to live. */
    protected int timeToLive = DEFAULT_TTL;
    
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
		 if (evt == null) {
			 throw new IllegalArgumentException("Event cannot be null nor empty");
	     }
		 String uid = KEY_EVENT + "-" + evt.getTimestamp() + "-" + evt.getUuid();
	     getJedis().set(uid, evt.toJson());
	     getJedis().persist(uid);
		 return true;
	}

	/** {@inheritDoc} */
	public Set<String> getFeatureNames() {
		
		// Cle composite EVENTYPE => EVENTNAME
		return null;
	}

	/** {@inheritDoc} */
	public int getTotalEventCount() {
		return 0;
	}

    /** {@inheritDoc} */
    public PieChart featuresListDistributionPie(long startTime, long endTime) {
        redisConnection.getJedis();
        return null;
    }

    /** {@inheritDoc} */
    public PieChart featureDistributionPie(String uid, long startTime, long endTime) {
        return null;
    }

    /** {@inheritDoc} */
    public BarChart getFeaturesUsageOverTime(Set<String> featNameSet, long startTime, long endTime, int nbslot) {
        return null;
    }
    
    /**
     * Safe acces to Jedis, avoid JNPE.
     *
     * @return
     */
    public Jedis getJedis() {
        if (redisConnection == null) {
            throw new IllegalArgumentException("Cannot found any redis Connection");
        }
        if (redisConnection.getJedis() == null) {
            throw new IllegalArgumentException("Cannot found any jedis connection, please build connection");
        }
        return redisConnection.getJedis();
    }

}