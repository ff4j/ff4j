package org.ff4j.services;

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
import org.ff4j.core.Feature;
import org.ff4j.services.domain.FeatureApiBean;
import org.ff4j.services.validator.FeatureValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@Service
public class GroupServices {
    @Autowired
    private FF4j ff4j;
    @Autowired
    private FeatureValidator featureValidator;

    public Collection<FeatureApiBean> getFeaturesByGroup(String groupName) {
        featureValidator.assertGroupExist(groupName);
        Collection<Feature> features = ff4j.getFeatureStore().readGroup(groupName).values();
        Collection<FeatureApiBean> featureApiBeans = new ArrayList<FeatureApiBean>();
        if (!CollectionUtils.isEmpty(features)) {
            for (Feature feature : features) {
                featureApiBeans.add(new FeatureApiBean(feature));
            }
        }
        return featureApiBeans;
    }

    public void enableGroup(String groupName) {
        featureValidator.assertGroupExist(groupName);
        ff4j.getFeatureStore().enableGroup(groupName);
    }

    public void disableGroup(String groupName) {
        featureValidator.assertGroupExist(groupName);
        ff4j.getFeatureStore().disableGroup(groupName);
    }
}
