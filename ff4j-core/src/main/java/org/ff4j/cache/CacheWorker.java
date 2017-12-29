package org.ff4j.cache;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import java.io.Serializable;

import org.ff4j.FF4jEntity;
import org.ff4j.repository.FF4jRepository;

/**
 * Worker invoke on fixed delay basis.
 * 
 * @author Cedrick LUNVEN  (@clunven)
 *
 * @param <K>
 *      cache key
 * @param <V>
 *      cache value
 */
public class CacheWorker< V extends FF4jEntity<?> > implements Runnable, Serializable {
    
    /** serialVersionUID. */
    private static final long serialVersionUID = 7679893286791023790L;
    
    /** cache manager to hold infos. */
    protected CacheManager< String, V > cacheManager;
    
    /** target ff4j repository (featureStore, propertyStore...). */
    protected FF4jRepository < String, V > ff4jRepository;
    
    /**
     * Default constructor.
     *
     * @param repo
     *      current repo
     * @param cacheManager
     *      current cache manager
     */
    public CacheWorker(FF4jRepository < String, V > repo, CacheManager< String, V > cacheManager) {
        this.cacheManager   = cacheManager;
        this.ff4jRepository = repo; 
    }

    /** {@inheritDoc} */
    @Override
    public void run() {
        if (ff4jRepository != null) {
            cacheManager.clear();
            ff4jRepository.findAll().forEach(f -> cacheManager.put(f.getUid(), f));
        }
    }
    
}
