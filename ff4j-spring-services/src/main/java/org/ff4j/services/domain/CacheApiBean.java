package org.ff4j.services.domain;

/*
 * #%L
 * ff4j-spring-services
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


import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.store.PropertyStore;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class CacheApiBean implements Serializable {

    private static final long serialVersionUID = -2564221971597313125L;

    private String cacheProvider;

    private String cacheStore;

    private Set<String> featureNames = new HashSet<String>();

    private Set<String> propertyNames = new HashSet<String>();

    public CacheApiBean() {
        super();
    }

    public CacheApiBean(FeatureStore featureStore) {
        if (featureStore instanceof FF4jCacheProxy) {
            FF4jCacheProxy cacheProxy = (FF4jCacheProxy) featureStore;
            cacheStore = cacheProxy.getCachedTargetStore();
            cacheProvider = cacheProxy.getCacheProvider();
            featureNames = cacheProxy.getCacheManager().listCachedFeatureNames();
        }
    }

    public CacheApiBean(PropertyStore propertyStore) {
        if (propertyStore instanceof FF4jCacheProxy) {
            FF4jCacheProxy cacheProxy = (FF4jCacheProxy) propertyStore;
            // FIXME : This is wrong. Need to ask Cedrick to change the implementation for property
            cacheStore = cacheProxy.getCachedTargetStore();
            cacheProvider = cacheProxy.getCacheProvider();
            propertyNames = cacheProxy.getCacheManager().listCachedPropertyNames();
        }
    }

    public Set<String> getPropertyNames() {
        return propertyNames;
    }

    public String getCacheProvider() {
        return cacheProvider;
    }

    public String getCacheStore() {
        return cacheStore;
    }

    public Set<String> getFeatureNames() {
        return featureNames;
    }

    public void setCacheProvider(String cacheProvider) {
        this.cacheProvider = cacheProvider;
    }

    public void setCacheStore(String cacheStore) {
        this.cacheStore = cacheStore;
    }

    public void setFeatureNames(Set<String> featureNames) {
        this.featureNames = featureNames;
    }

    public void setPropertyNames(Set<String> propertyNames) {
        this.propertyNames = propertyNames;
    }
}
