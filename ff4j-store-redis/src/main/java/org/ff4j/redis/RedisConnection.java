package org.ff4j.redis;

/*
 * #%L
 * ff4j-store-redis
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;
import redis.clients.jedis.JedisSentinelPool;
import redis.clients.jedis.Protocol;
import redis.clients.util.Pool;

/**
 * Connection to redis DataBase.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class RedisConnection {
  
    /** redis host(if not sentinel). */
    protected String redisHost = Protocol.DEFAULT_HOST;

    /** redis port (if not sentinel). */
    protected int redisport =  Protocol.DEFAULT_PORT;
  
    /** used in protected redis cluster. */
    protected String redisPassword = null;
    
    /** redis. */ 
    protected int redisPoolMaxTotal =  8;
    
    /** redis. */ 
    protected int redisPoolTimeout = Protocol.DEFAULT_TIMEOUT;
    
    /**
     * Jedis connection Pool.
     * 
     * @see JedisPool
     * @see JedisSentinelPool
     */
    protected Pool < Jedis > jedisPool;
    
    /**
     * Default constructor.
     */
    public RedisConnection() {
    }
    
    /** Jedis connection Pool. */
    public RedisConnection(Pool < Jedis > jedisPool) {
        this.jedisPool = jedisPool;
    }
    
    /**
     * Constructor for unauthenticated connection.
     *
     * @param predisHost
     * 		host
     * @param predisPort
     * 		port
     */
    public RedisConnection(String predisHost, int predisPort) {
        this.redisHost      = predisHost;
        this.redisport      = predisPort;
    }
    
    /**
     * Constructor for authenticated connection.
     *
     * @param predisHost
     * 		hostname
     * @param predisPort
     * 		port
     * @param password
     * 		redis password
     */
    public RedisConnection(String predisHost, int predisPort, String password) {
        this(predisHost, predisPort);
        this.redisPassword  = password;
    }
    
    /**
     * Getter accessor for attribute 'redisHost'.
     * 
     * @return current value of 'redisHost'
     */
    public String getRedisHost() {
        return redisHost;
    }

    /**
     * Getter accessor for attribute 'redisport'.
     * 
     * @return current value of 'redisport'
     */
    public int getRedisport() {
        return redisport;
    }

    /**
     * Getter accessor for attribute 'redisPassword'.
     *
     * @return the redisPassword
     */
    @Deprecated
    public String getRedisPassword() {
        return redisPassword;
    }
    
    /**
     * Getter accessor for attribute 'jedis'.
     *
     * @return
     *       current value of 'jedis'
     */
    public Jedis getJedis() {
        if (jedisPool == null) {
            if (redisPassword != null && !"".equals(redisPassword)) {
                JedisPoolConfig poolConfig = new JedisPoolConfig();
                this.jedisPool = new JedisPool(poolConfig, 
                        redisHost, redisport, redisPoolTimeout, redisPassword);
            } else {
                this.jedisPool = new JedisPool(redisHost, redisport);
            }
        }
        return jedisPool.getResource();
    }
    
    /**
     * Clean up connections after tests.
     */
    public void destroyPool() {
        if (jedisPool != null) {
            jedisPool.destroy();
        }
    }

	/**
	 * Setter accessor for attribute 'redisPassword'.
	 * @param redisPassword the redisPassword to set
	 */
	public void setRedisPassword(String redisPassword) {
		this.redisPassword = redisPassword;
	}

	/**
	 * Setter accessor for attribute 'redisHost'.
	 *
	 * @param redisHost the redisHost to set
	 */
	public void setRedisHost(String redisHost) {
		this.redisHost = redisHost;
	}

	/**
	 * Setter accessor for attribute 'redisport'.
	 *
	 * @param redisport the redisport to set
	 */
	public void setRedisport(int redisport) {
		this.redisport = redisport;
	}

    /**
     * Getter accessor for attribute 'redisPoolMaxTotal'.
     *
     * @return
     *       current value of 'redisPoolMaxTotal'
     */
    public int getRedisPoolMaxTotal() {
        return redisPoolMaxTotal;
    }

    /**
     * Setter accessor for attribute 'redisPoolMaxTotal'.
     * @param redisPoolMaxTotal
     * 		new value for 'redisPoolMaxTotal '
     */
    public void setRedisPoolMaxTotal(int redisPoolMaxTotal) {
        this.redisPoolMaxTotal = redisPoolMaxTotal;
    }

    /**
     * Getter accessor for attribute 'redisPoolTimeout'.
     *
     * @return
     *       current value of 'redisPoolTimeout'
     */
    public long getRedisPoolTimeout() {
        return redisPoolTimeout;
    }

    /**
     * Setter accessor for attribute 'redisPoolTimeout'.
     * @param redisPoolTimeout
     * 		new value for 'redisPoolTimeout '
     */
    public void setRedisPoolTimeout(int redisPoolTimeout) {
        this.redisPoolTimeout = redisPoolTimeout;
    }

    /**
     * Getter accessor for attribute 'jedisPool'.
     *
     * @return
     *       current value of 'jedisPool'
     */
    public Pool<Jedis> getJedisPool() {
        return jedisPool;
    }

    /**
     * Setter accessor for attribute 'jedisPool'.
     * @param jedisPool
     * 		new value for 'jedisPool '
     */
    public void setJedisPool(Pool<Jedis> jedisPool) {
        this.jedisPool = jedisPool;
    }

}
