package org.ff4j.dynamodb;

/*
 * #%L
 * ff4j-store-aws-dynamodb
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

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.document.*;
import com.amazonaws.services.dynamodbv2.document.spec.GetItemSpec;
import com.amazonaws.services.dynamodbv2.document.spec.QuerySpec;
import com.amazonaws.services.dynamodbv2.document.spec.ScanSpec;
import com.amazonaws.services.dynamodbv2.document.spec.UpdateItemSpec;
import com.amazonaws.services.dynamodbv2.document.utils.NameMap;
import com.amazonaws.services.dynamodbv2.document.utils.ValueMap;
import com.amazonaws.services.dynamodbv2.model.*;
import com.amazonaws.util.CollectionUtils;
import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.utils.Util;

import java.util.*;

import static org.ff4j.dynamodb.DynamoDBConstants.*;

/**
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
class DynamoDBClient {

    private final DynamoDBPropertyMapper PROPERTY_MAPPER = new DynamoDBPropertyMapper();
    private final DynamoDBFeatureMapper FEATURE_MAPPER = new DynamoDBFeatureMapper();

    private final AmazonDynamoDB amazonDynamoDB;
    private final DynamoDB dynamoDB;
    private final String tableName;
    private final String key;
    private Table table;

    DynamoDBClient(AmazonDynamoDB amazonDynamoDB, String tableName) {
        this.amazonDynamoDB = amazonDynamoDB;
        this.dynamoDB = new DynamoDB(amazonDynamoDB);
        this.tableName = tableName;
        this.table = dynamoDB.getTable(tableName);

        if (PROPERTY_TABLE_NAME.equals(tableName)) {
            key = PROPERTY_NAME;
        } else if (FEATURE_TABLE_NAME.equals(tableName)) {
            key = FEATURE_UID;
        } else {
            throw new UnsupportedOperationException(tableName + " unknown");
        }
    }

    void putFeature(Feature feature) {
        table.putItem(FEATURE_MAPPER.toStore(feature));
    }

    void putProperty(Property property) {
        table.putItem(PROPERTY_MAPPER.toStore(property));
    }

    void updateProperty(String propName, String newValue) {
        table.updateItem(new PrimaryKey(PROPERTY_NAME, propName), new AttributeUpdate(PROPERTY_VALUE).put(newValue));
    }

    Property getProperty(String name) {
        Item item = getItem(name);
        return PROPERTY_MAPPER.fromStore(item);
    }

    Feature getFeature(String featureUid) {
        Item item = getItem(featureUid);
        return FEATURE_MAPPER.fromStore(item);
    }

    void delete(String id) {
        table.deleteItem(new KeyAttribute(key, id));
    }

    Map<String, Feature> getAllFeatures() {
        ItemCollection<ScanOutcome> items = table.scan(new ScanSpec().withSelect(Select.ALL_ATTRIBUTES));
        Map<String, Feature> map = new HashMap<String, Feature>();

        for (Item item : items) {
            map.put(item.getString(FEATURE_UID), FEATURE_MAPPER.fromStore(item));
        }

        return map;
    }

    Map<String, Property<?>> getAllProperties() {
        ItemCollection<ScanOutcome> items = table.scan(new ScanSpec().withSelect(Select.ALL_ATTRIBUTES));
        Map<String, Property<?>> map = new HashMap<String, Property<?>>();

        for (Item item : items) {
            map.put(item.getString(PROPERTY_NAME), PROPERTY_MAPPER.fromStore(item));
        }

        return map;
    }

    Set<String> getAllGroups() {
        ItemCollection<ScanOutcome> items = table.scan(new ScanSpec().withSelect(Select.SPECIFIC_ATTRIBUTES).withAttributesToGet(FEATURE_GROUP));
        Set<String> groupNames = new HashSet<String>();

        for (Item item : items) {
            if (item.getString(FEATURE_GROUP) != null) {
                groupNames.add(item.getString(FEATURE_GROUP));
            }
        }

        return groupNames;
    }

    Set<String> getAllNames() {
        ItemCollection<ScanOutcome> items = table.scan(new ScanSpec().withAttributesToGet(key));

        Set<String> names = new HashSet<String>();
        for (Item item : items) {
            names.add(item.getString(key));
        }
        return names;
    }

    Item getItem(String id) {
        Util.assertHasLength(id);

        Item item = table.getItem(new GetItemSpec().withPrimaryKey(new PrimaryKey(key, id)));
        if (item == null) {
            throw key.equals(FEATURE_UID) ? new FeatureNotFoundException(id) : new PropertyNotFoundException(id);
        }
        return item;
    }

    Map<String, Feature> getFeaturesByGroup(String group) {
        ItemCollection<QueryOutcome> items = getItemsByGroup(group);
        Map<String, Feature> map = new HashMap<String, Feature>();

        for (Item item : items) {
            map.put(item.getString(FEATURE_UID), FEATURE_MAPPER.fromStore(item));
        }
        return map;
    }

    ItemCollection<QueryOutcome> getItemsByGroup(String group) {
        Index index = table.getIndex(FEATURE_GROUP_INDEX);
        QuerySpec spec = new QuerySpec()
                .withKeyConditionExpression(FEATURE_GROUP + " = :v_group")
                .withValueMap(new ValueMap()
                        .withString(":v_group", group));

        ItemCollection<QueryOutcome> items = index.query(spec);
        if (items == null || !items.iterator().hasNext()) {
            throw new GroupNotFoundException(group);
        }
        return items;
    }

    void updateFeatureAvailability(String featUid, boolean enable) {
        UpdateItemSpec updateItemSpec = new UpdateItemSpec()
                .withPrimaryKey(key, featUid)
                .withUpdateExpression("set #en = :val1")
                .withNameMap(new NameMap().with("#en", FEATURE_ENABLE))
                .withValueMap(new ValueMap().withBoolean(":val1", enable))
                .withReturnValues(ReturnValue.NONE);
        table.updateItem(updateItemSpec);
    }

    void updateFeatureAvailabilityInGroup(String group, boolean enable) {
        ItemCollection<QueryOutcome> items = getItemsByGroup(group);
        if (items == null || !items.iterator().hasNext()) {
            throw new GroupNotFoundException(group);
        }
        for (Item item : items) {
            updateFeatureAvailability(item.getString(FEATURE_UID), enable);
        }
    }

    void addToGroup(String featUid, String group) {
        UpdateItemSpec updateItemSpec = new UpdateItemSpec()
                .withPrimaryKey(key, featUid)
                .withAttributeUpdate(new AttributeUpdate(FEATURE_GROUP).put(group))
                .withReturnValues(ReturnValue.NONE);
        table.updateItem(updateItemSpec);
    }

    void removeFromGroup(String featUid) {
        UpdateItemSpec updateItemSpec = new UpdateItemSpec()
                .withPrimaryKey(key, featUid)
                .withAttributeUpdate(new AttributeUpdate(FEATURE_GROUP).delete())
                .withReturnValues(ReturnValue.NONE);
        table.updateItem(updateItemSpec);
    }



    /**
     * TODO : customize table (throughput...)
     */
    void createPropertyTable() {
        CreateTableRequest request = new CreateTableRequest()
                .withAttributeDefinitions(
                        new AttributeDefinition(PROPERTY_NAME, ScalarAttributeType.S)
                )
                .withKeySchema(new KeySchemaElement(PROPERTY_NAME, KeyType.HASH))
                .withProvisionedThroughput(new ProvisionedThroughput(10L, 10L))
                .withTableName(tableName);

        try {
            table = dynamoDB.createTable(request);
            table.waitForActive();
        } catch (Exception e) {
            throw new IllegalStateException("Cannot initialize Property Table in DynamoDB", e);
        }
    }

    /**
     * TODO : customize table (throughput...)
     */
    void createFeatureTable() {
        CreateTableRequest request = new CreateTableRequest()
                .withAttributeDefinitions(
                        new AttributeDefinition(FEATURE_UID, ScalarAttributeType.S),
                        new AttributeDefinition(FEATURE_GROUP, ScalarAttributeType.S)
                )
                .withKeySchema(new KeySchemaElement(FEATURE_UID, KeyType.HASH))
                .withProvisionedThroughput(new ProvisionedThroughput(10L, 10L))
                .withGlobalSecondaryIndexes(
                        new GlobalSecondaryIndex()
                                .withIndexName(FEATURE_GROUP_INDEX)
                                .withKeySchema(new KeySchemaElement(FEATURE_GROUP, KeyType.HASH))
                                .withProjection(new Projection().withProjectionType(ProjectionType.ALL))
                                .withProvisionedThroughput(new ProvisionedThroughput(10L, 10L))

                )
                .withTableName(tableName);

        try {
            table = dynamoDB.createTable(request);
            table.waitForActive();
        } catch (Exception e) {
            throw new IllegalStateException("Cannot initialize Property Table in DynamoDB", e);
        }
    }

    boolean tableExists() {
        try {
            amazonDynamoDB.describeTable(tableName);
            table = dynamoDB.getTable(tableName);
        } catch (ResourceNotFoundException e) {
            return false;
        }
        return true;
    }


    void deleteTable() {
        table.delete();
        try {
            table.waitForDelete();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

}
