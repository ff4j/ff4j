/*
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * Copyright 2013-2016 the original author or authors.
 */

package org.ff4j.spring.boot.domain;

/*
 * #%L
 * ff4j-spring-boot-starter
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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class FeatureStoreApiBean implements Serializable {

    private static final long serialVersionUID = 1868920596870427435L;

    private String type;

    private int numberOfFeatures;

    private int numberOfGroups;

    private List<String> features = new ArrayList<>();

    private List<String> groups = new ArrayList<>();

    private CacheApiBean cache;

    public FeatureStoreApiBean(FeatureStore featureStore) {
        type = featureStore.getClass().getCanonicalName();
        if (featureStore instanceof FF4jCacheProxy) {
            cache = new CacheApiBean(featureStore);
        }
        features = new ArrayList<>(featureStore.readAll().keySet());
        groups = new ArrayList<>(featureStore.readAllGroups());
        numberOfFeatures = features.size();
        numberOfGroups = groups.size();
    }

    public String getType() {
        return type;
    }

    public int getNumberOfFeatures() {
        return numberOfFeatures;
    }

    public int getNumberOfGroups() {
        return numberOfGroups;
    }

    public List<String> getFeatures() {
        return features;
    }

    public List<String> getGroups() {
        return groups;
    }

    public CacheApiBean getCache() {
        return cache;
    }
}
