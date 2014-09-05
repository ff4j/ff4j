package org.ff4j.redis;

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

import redis.clients.jedis.Jedis;

/**
 * Parent class to work with Jedis.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class AbstractRedisProvider {

    /** prefix of keys. */
    public static final String PREFIX_KEY = "FF4J_";
    
    /** default host. */
    public static final String DEFAULT_REDIS_HOST = "localhost";

    /** default port. */
    public static final int DEFAULT_REDIS_PORT = 6379;

    /** default ttl. */
    public static final int DEFAULT_TTL = 900000000;

    /** redis host. */
    protected String redisHost = DEFAULT_REDIS_HOST;

    /** redis port. */
    protected int redisport = DEFAULT_REDIS_PORT;

    /** time to live. */
    protected int timeToLive = DEFAULT_TTL;

    /** Java Redis CLIENT. */
    protected Jedis jedis;
}
