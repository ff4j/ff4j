package org.ff4j.apache.ig.store;

/*
 * #%L
 * ff4j-store-apache-ignite
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

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import javax.cache.Cache;
import org.apache.ignite.IgniteCache;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;

/**
 * Implementation of {@link FeatureStore} for apache ignite.
 *
 */
public class FeatureStoreApacheIgnite extends AbstractFeatureStore {

    private IgniteCache<String, Feature> igniteCache;

    public FeatureStoreApacheIgnite(IgniteCache<String, Feature> igniteCache){
        this.igniteCache = igniteCache;
    }

    @Override
    public boolean exist(String featId) {
        Util.assertParamHasLength(featId, "Feature identifier");
        return igniteCache.containsKey(featId);
    }

    @Override
    public void create(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        igniteCache.put(fp.getUid(),fp);
    }

    @Override
    public Feature read(String featureUid) {
        if (!exist(featureUid)) {
            throw new FeatureNotFoundException(featureUid);
        }
        return igniteCache.get(featureUid);
    }

    @Override
    public Map<String, Feature> readAll() {
         return StreamSupport.stream(igniteCache.spliterator(), false)
                .collect(Collectors.toMap(Cache.Entry::getKey, Cache.Entry::getValue));
    }

    @Override
    public void delete(String fpId) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        igniteCache.remove(fpId);
    }

    @Override
    public void update(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null");
        }
        if (!exist(fp.getUid())) {
            throw new FeatureNotFoundException(fp.getUid());
        }
        igniteCache.put(fp.getUid(), fp);
    }

    @Override
    public void clear() {
        igniteCache.clear();
    }

}
