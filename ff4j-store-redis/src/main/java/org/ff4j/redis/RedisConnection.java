package org.ff4j.redis;

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
