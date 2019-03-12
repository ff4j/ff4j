package org.ff4j.dynamodb.feature;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.document.*;
import com.amazonaws.services.dynamodbv2.document.spec.QuerySpec;
import com.amazonaws.services.dynamodbv2.document.spec.ScanSpec;
import com.amazonaws.services.dynamodbv2.document.spec.UpdateItemSpec;
import com.amazonaws.services.dynamodbv2.document.utils.NameMap;
import com.amazonaws.services.dynamodbv2.document.utils.ValueMap;
import com.amazonaws.services.dynamodbv2.model.*;
import org.ff4j.core.Feature;
import org.ff4j.dynamodb.DynamoDBClient;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.ff4j.dynamodb.DynamoDBConstants.*;

public class FeatureDynamoDBClient extends DynamoDBClient<Feature> {

    private final FeatureDynamoDBMapper FEATURE_MAPPER = new FeatureDynamoDBMapper();

    FeatureDynamoDBClient(AmazonDynamoDB amazonDynamoDB, String tableName) {
        super(amazonDynamoDB, tableName);
        key = FEATURE_UID;
    }

    @Override
    protected RuntimeException notFoundException(String id) {
        return new FeatureNotFoundException(id);
    }

    @Override
    protected void put(Feature feature) {
        table.putItem(FEATURE_MAPPER.toStore(feature));
    }

    @Override
    protected Feature get(String featureUid) {
        Item item = getItem(featureUid);
        return FEATURE_MAPPER.fromStore(item);
    }

    @Override
    protected Map<String, Feature> getAll() {
        ItemCollection<ScanOutcome> items = table.scan(new ScanSpec().withSelect(Select.ALL_ATTRIBUTES));
        Map<String, Feature> map = new HashMap<String, Feature>();

        for (Item item : items) {
            map.put(item.getString(FEATURE_UID), FEATURE_MAPPER.fromStore(item));
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
    @Override
    protected void createTable() {
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
}
