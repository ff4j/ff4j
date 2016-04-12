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
import org.ff4j.services.exceptions.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@Component
public class FeatureValidator {
    @Autowired
    private FF4j ff4j;

    public void assertFeatureIdsMatch(String featureUID, String featureApiUID) {
        if (!featureUID.equals(featureApiUID)) {
            throw new FeatureIdNotMatchException();
        }
    }

    public void assertFeatureUIDIsNotBlank(String featureUID) {
        if (StringUtils.isBlank(featureUID)) {
            throw new FeatureIdBlankException();
        }
    }

    public void assertFeatureExists(String featureUID) {
        if (!ff4j.exist(featureUID)) {
            throw new FeatureNotFoundException();
        }
    }

    public void assertRoleDoesNotExist(String featureUID, String role) {
        if (ff4j.getFeatureStore().read(featureUID).getPermissions().contains(role)) {
            throw new RoleExistsException();
        }
    }

    public void assertRoleExist(String featureUID, String role) {
        if (!ff4j.getFeatureStore().read(featureUID).getPermissions().contains(role)) {
            throw new RoleNotExistsException();
        }
    }

    public void assertGroupDoesNotExist(String groupName) {
        if (ff4j.getFeatureStore().existGroup(groupName)) {
            throw new GroupExistsException();
        }
    }

    public void assertGroupExist(String groupName) {
        if (!ff4j.getFeatureStore().existGroup(groupName)) {
            throw new GroupNotExistsException();
        }
    }
}
