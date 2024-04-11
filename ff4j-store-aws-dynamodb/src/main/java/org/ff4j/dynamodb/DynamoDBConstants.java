package org.ff4j.dynamodb;

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

/**
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public interface DynamoDBConstants {
    String CONFIG_FILE = "ff4j-dynamodb.properties";
    String CONFIG_PROPERTY_TABLE_NAME = "ff4j.store.dynamodb.property.table.name";
    String CONFIG_PROPERTY_BILLING = "ff4j.store.dynamodb.property.table.billing";
    String CONFIG_PROPERTY_RCU = "ff4j.store.dynamodb.property.table.billing.rcu";
    String CONFIG_PROPERTY_WCU = "ff4j.store.dynamodb.property.table.billing.wcu";
    String CONFIG_FEATURE_TABLE_NAME = "ff4j.store.dynamodb.feature.table.name";
    String CONFIG_FEATURE_BILLING = "ff4j.store.dynamodb.feature.table.billing";
    String CONFIG_FEATURE_RCU = "ff4j.store.dynamodb.feature.table.billing.rcu";
    String CONFIG_FEATURE_WCU = "ff4j.store.dynamodb.feature.table.billing.wcu";
    long DEFAULT_RCU = 5;
    long DEFAULT_WCU = 5;

    String PROPERTY_TABLE_NAME = "ff4j-properties";
    String PROPERTY_NAME = "name";
    String PROPERTY_VALUE = "currentValue";
    String PROPERTY_VALUES = "fixedValues";
    String PROPERTY_DESCRIPTION = "description";
    String PROPERTY_TYPE = "type";


    String FEATURE_TABLE_NAME = "ff4j-features";
    String FEATURE_GROUP_INDEX = "ff4j-feature-groups";
    String FEATURE_UID = "featureUid";
    String FEATURE_GROUP = "groupName";
    String FEATURE_ENABLE = "enable";
    String FEATURE_DESCRIPTION = "description";
    String FEATURE_STRATEGY = "strategy";
    String FEATURE_ROLE = "role";
    String FEATURE_PROPERTIES = "properties";
}

