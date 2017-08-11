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
import org.ff4j.property.store.PropertyStore;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class PropertyStoreApiBean implements Serializable {

    private static final long serialVersionUID = 5459281574635411541L;

    private String type;

    private int numberOfProperties;

    private Set<String> properties = new HashSet<String>();

    private CacheApiBean cache;

    public PropertyStoreApiBean() {
        super();
    }

    public PropertyStoreApiBean(PropertyStore pStore) {
        type = pStore.getClass().getName();
        if (pStore instanceof FF4jCacheProxy) {
            cache = new CacheApiBean(pStore);
        }
        properties = pStore.listPropertyNames();
        numberOfProperties = properties.size();
    }

    public String getType() {
        return type;
    }

    public int getNumberOfProperties() {
        return numberOfProperties;
    }

    public Set<String> getProperties() {
        return properties;
    }

    public CacheApiBean getCache() {
        return cache;
    }

    public void setType(String type) {
        this.type = type;
    }

    public void setNumberOfProperties(int numberOfProperties) {
        this.numberOfProperties = numberOfProperties;
    }

    public void setProperties(Set<String> properties) {
        this.properties = properties;
    }

    public void setCache(CacheApiBean cache) {
        this.cache = cache;
    }
}
