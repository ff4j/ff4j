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
import org.ff4j.property.Property;
import org.ff4j.services.domain.PropertyApiBean;
import org.ff4j.services.model.FeatureActions;
import org.ff4j.services.validator.PropertyValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@Service
public class PropertyServices {
    @Autowired
    private FF4j ff4j;
    @Autowired
    private PropertyValidator propertyValidator;

    public PropertyApiBean getProperty(String propertyName) {
        propertyValidator.assertPropertyExist(propertyName);
        return new PropertyApiBean(ff4j.getPropertiesStore().readProperty(propertyName));
    }

    public FeatureActions createOrUpdateProperty(String propertyName, PropertyApiBean propertyApiBean) {
        propertyValidator.assertPropertyNameNotBlank(propertyApiBean.getName());
        propertyValidator.assertPropertyNameMatch(propertyName, propertyApiBean.getName());
        Property<?> property = propertyApiBean.asProperty();
        if (ff4j.getPropertiesStore().existProperty(propertyName)) {
            ff4j.getPropertiesStore().updateProperty(property);
            return FeatureActions.UPDATED;
        } else {
            ff4j.getPropertiesStore().createProperty(property);
            return FeatureActions.CREATED;
        }
    }

    public void deleteProperty(String propertyName) {
        propertyValidator.assertPropertyExist(propertyName);
        ff4j.getPropertiesStore().deleteProperty(propertyName);
    }

    public void updatePropertyName(String propertyName, String newValue) {
        propertyValidator.assertPropertyExist(propertyName);
        ff4j.getPropertiesStore().updateProperty(propertyName, newValue);
    }
}
