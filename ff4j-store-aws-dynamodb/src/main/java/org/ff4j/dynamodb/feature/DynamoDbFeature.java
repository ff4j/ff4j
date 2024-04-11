package org.ff4j.dynamodb.feature;

/*-
 * #%L
 * ff4j-store-aws-dynamodb
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import software.amazon.awssdk.enhanced.dynamodb.mapper.annotations.DynamoDbBean;
import software.amazon.awssdk.enhanced.dynamodb.mapper.annotations.DynamoDbPartitionKey;
import software.amazon.awssdk.enhanced.dynamodb.mapper.annotations.DynamoDbSecondaryPartitionKey;

import java.util.Map;
import java.util.Set;

import static org.ff4j.dynamodb.DynamoDBConstants.FEATURE_GROUP_INDEX;

/**
 * See {@link org.ff4j.core.Feature for more details}
 */
@DynamoDbBean
public class DynamoDbFeature {
    /** Unique DynamoDbFeature Identifier */
    private String featureUid;

    @DynamoDbPartitionKey
    public String getFeatureUid() {
        return featureUid;
    }

    public void setFeatureUid(String featureUid) {
        this.featureUid = featureUid;
    }

    private boolean enable = false;

    private String description;

    private String groupName;
    
    private String flippingStrategy;

    private Set<String> permissions;

    private Map<String, String> properties;

    public boolean isEnable() {
        return enable;
    }

    public void setEnable(boolean enable) {
        this.enable = enable;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @DynamoDbSecondaryPartitionKey(indexNames = {FEATURE_GROUP_INDEX})
    public String getGroupName() {
        return groupName;
    }

    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }

    public void setFlippingStrategy(String flippingStrategy) {
        this.flippingStrategy = flippingStrategy;
    }

    public String getFlippingStrategy() {
        return flippingStrategy;
    }

    public void setPermissions(Set<String> permissions) {
        this.permissions = permissions;
    }

    public Set<String> getPermissions() {
        return permissions;
    }

    public void setProperties(Map<String, String> properties) {
        this.properties = properties;
    }

    public Map<String, String> getProperties() {
        return properties;
    }
}
