package org.ff4j.services.validator;

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

import org.apache.commons.lang3.StringUtils;
import org.ff4j.FF4j;
import org.ff4j.services.exceptions.PropertyNameBlankException;
import org.ff4j.services.exceptions.PropertyNameNotMatchException;
import org.ff4j.services.exceptions.PropertyNotFoundException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@Component
public class PropertyValidator {
    @Autowired
    private FF4j ff4j;


    public void assertPropertyExist(String propertyName) {
        if (!ff4j.getPropertiesStore().existProperty(propertyName)) {
            throw new PropertyNotFoundException();
        }
    }

    public void assertPropertyNameNotBlank(String name) {
        if (StringUtils.isBlank(name)) {
            throw new PropertyNameBlankException();
        }
    }

    public void assertPropertyNameMatch(String propertyName, String propertyApiBeanName) {
        if (!propertyName.equalsIgnoreCase(propertyApiBeanName)) {
            throw new PropertyNameNotMatchException();
        }
    }
}
