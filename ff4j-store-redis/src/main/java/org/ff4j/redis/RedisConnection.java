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

public class RedisConnection {
    
    /** default host. */
    private static String DEFAULT_REDIS_HOST = "localhost";

    /** default port. */
    private static int DEFAULT_REDIS_PORT = 6379;

 
    
    /** redis host. */
    protected String redisHost = DEFAULT_REDIS_HOST;

    /** redis port. */
    protected int redisport = DEFAULT_REDIS_PORT;
    
    /** Java Redis CLIENT. */
    protected Jedis jedis;
    
    public RedisConnection() {
        this(DEFAULT_REDIS_HOST, DEFAULT_REDIS_PORT);
    }
    
    public RedisConnection(String redisHost, int redisPort) {
        this.redisHost = redisHost;
        this.redisport = redisPort;
        jedis = new Jedis(redisHost, redisport);
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
     * Getter accessor for attribute 'jedis'.
     *
     * @return
     *       current value of 'jedis'
     */
    public Jedis getJedis() {
        return jedis;
    }
    
    

}
