package org.ff4j.services;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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

import org.ff4j.FF4j;
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.property.Property;
import org.ff4j.services.domain.CacheApiBean;
import org.ff4j.services.domain.PropertyApiBean;
import org.ff4j.services.domain.PropertyStoreApiBean;
import org.ff4j.services.exceptions.PropertyStoreNotCached;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@Service
public class PropertyStoreServices {

    @Autowired
    private FF4j ff4j;

    public PropertyStoreApiBean getPropertyStore() {
        return new PropertyStoreApiBean(ff4j.getPropertiesStore());
    }

    @SuppressWarnings("rawtypes")
    public List<PropertyApiBean> getAllProperties() {
        List<PropertyApiBean> properties;
        Map<String, Property<?>> propertyMap = ff4j.getPropertiesStore().readAllProperties();
        if (CollectionUtils.isEmpty(propertyMap)) {
            properties = new ArrayList<PropertyApiBean>(0);
        } else {
            properties = new ArrayList<PropertyApiBean>(propertyMap.size());
            for (Property value : propertyMap.values()) {
                properties.add(new PropertyApiBean(value));
            }
        }
        return properties;
    }

    public void deleteAllProperties() {
        ff4j.getPropertiesStore().clear();
    }

    public CacheApiBean getPropertiesFromCache() {
        FF4jCacheProxy cacheProxy = ff4j.getCacheProxy();
        if (cacheProxy == null) {
            throw new PropertyStoreNotCached();
        }
        return new CacheApiBean(ff4j.getPropertiesStore());
    }

    public void clearCachedPropertyStore() {
        FF4jCacheProxy cacheProxy = ff4j.getCacheProxy();
        if (cacheProxy == null) {
            throw new PropertyStoreNotCached();
        }
        cacheProxy.getCacheManager().clearProperties();
    }
}
