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

import org.ff4j.core.Feature;
import org.ff4j.dynamodb.DynamoDBClient;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import software.amazon.awssdk.core.pagination.sync.SdkIterable;
import software.amazon.awssdk.enhanced.dynamodb.Key;
import software.amazon.awssdk.enhanced.dynamodb.TableSchema;
import software.amazon.awssdk.enhanced.dynamodb.model.Page;
import software.amazon.awssdk.enhanced.dynamodb.model.PageIterable;
import software.amazon.awssdk.enhanced.dynamodb.model.WriteBatch;
import software.amazon.awssdk.services.dynamodb.DynamoDbClient;
import software.amazon.awssdk.services.dynamodb.model.*;
import software.amazon.awssdk.utils.CollectionUtils;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static org.ff4j.dynamodb.DynamoDBConstants.*;
import static software.amazon.awssdk.enhanced.dynamodb.model.QueryConditional.keyEqualTo;

/**
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public class FeatureDynamoDBClient extends DynamoDBClient<Feature, DynamoDbFeature> {

    private final FeatureDynamoDBMapper FEATURE_MAPPER = new FeatureDynamoDBMapper();


    FeatureDynamoDBClient(DynamoDbClient client) {
        super(client);
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
        DynamoDbFeature item = getItem(featureUid);
        return FEATURE_MAPPER.fromStore(item);
    }

    @Override
    protected Map<String, Feature> getAll() {
        PageIterable<DynamoDbFeature> pages = table.scan();

        Map<String, Feature> map = new HashMap<>();
        pages.items().forEach(dynamoDbFeature -> map.put(dynamoDbFeature.getFeatureUid(), FEATURE_MAPPER.fromStore(dynamoDbFeature)));

        return map;
    }

    Set<String> getAllGroups() {
        SdkIterable<Page<DynamoDbFeature>> pages = table.index(FEATURE_GROUP_INDEX).scan(builder -> builder.attributesToProject(FEATURE_GROUP));

        Set<String> groupNames = new HashSet<>();
        pages.forEach(p -> p.items().forEach(f -> groupNames.add(f.getGroupName())));

        return groupNames;
    }

    Map<String, Feature> getFeaturesByGroup(String group) {
        SdkIterable<Page<DynamoDbFeature>> items = getItemsByGroup(group);
        Map<String, Feature> map = new HashMap<>();

        items.forEach(
                dynamoDbFeaturePage -> dynamoDbFeaturePage.items().forEach(
                        dynamoDbFeature -> map.put(dynamoDbFeature.getFeatureUid(), FEATURE_MAPPER.fromStore(dynamoDbFeature))
                )
        );

        return map;
    }

    SdkIterable<Page<DynamoDbFeature>> getItemsByGroup(String group) {
        SdkIterable<Page<DynamoDbFeature>> pages = table.index(FEATURE_GROUP_INDEX).query(r -> r.queryConditional(keyEqualTo(k -> k.partitionValue(group))));

        if (pages == null || pages.stream().count() == 0 || pages.stream().allMatch(p -> p.items().size() == 0)) {
            throw new GroupNotFoundException(group);
        }
        return pages;
    }

    void updateFeatureAvailability(String featUid, boolean enable) {
        DynamoDbFeature item = getItem(featUid);
        if (item.isEnable() == enable) {
            return;
        }
        item.setEnable(enable);
        table.updateItem(item);
    }

    void updateFeatureAvailabilityInGroup(String group, boolean enable) {
        SdkIterable<Page<DynamoDbFeature>> itemsByGroup = getItemsByGroup(group);

        itemsByGroup.forEach(
                dynamoDbFeaturePage -> dynamoDbFeaturePage.items().forEach(
                        dynamoDbFeature -> {
                            if (dynamoDbFeature.isEnable() != enable) {
                                dynamoDbFeature.setEnable(enable);
                                table.updateItem(dynamoDbFeature); // TODO: batchUpdate when it will exist in DDB
                            }
                        }
                )
        );
    }

    void addFeaturePermission(String featUid, String roleName) {
        DynamoDbFeature item = getItem(featUid);
        if (item.getPermissions() == null) {
            item.setPermissions(new HashSet<>());
        }
        item.getPermissions().add(roleName);
        table.updateItem(item);
    }

    void removeFeaturePermission(String featUid, String roleName) {
        DynamoDbFeature item = getItem(featUid);
        if (!CollectionUtils.isNullOrEmpty(item.getPermissions())) {
            item.getPermissions().remove(roleName);
            if (item.getPermissions().size() == 0) {
                item.setPermissions(null);
            }
        }
        table.updateItem(item);
    }

    void addToGroup(String featUid, String group) {
        DynamoDbFeature item = getItem(featUid);
        item.setGroupName(group);
        table.updateItem(item);
    }

    void removeFromGroup(String featUid) {
        DynamoDbFeature item = getItem(featUid);
        item.setGroupName(null);
        table.updateItem(item);
    }

    @Override
    protected void loadProperties(Properties prop) {
        tableName = prop.getProperty(CONFIG_FEATURE_TABLE_NAME, FEATURE_TABLE_NAME);

        String billing = prop.getProperty(CONFIG_FEATURE_BILLING, BillingMode.PROVISIONED.toString());
        this.billingMode = BillingMode.valueOf(billing);

        String rcu = prop.getProperty(CONFIG_FEATURE_RCU, String.valueOf(DEFAULT_RCU));
        this.billingRCU = Long.valueOf(rcu);

        String wcu = prop.getProperty(CONFIG_FEATURE_WCU, String.valueOf(DEFAULT_WCU));
        this.billingWCU = Long.valueOf(wcu);
    }

    @Override
    protected void createTable() {
        GlobalSecondaryIndex.Builder globalSecondaryIndexBuilder = GlobalSecondaryIndex.builder()
                .indexName(FEATURE_GROUP_INDEX)
                .keySchema(KeySchemaElement.builder().attributeName(FEATURE_GROUP).keyType(KeyType.HASH).build())
                .projection(Projection.builder().projectionType(ProjectionType.ALL).build());

        if (BillingMode.PROVISIONED.equals(billingMode)) {
            globalSecondaryIndexBuilder.provisionedThroughput(ProvisionedThroughput.builder().readCapacityUnits(billingRCU).writeCapacityUnits(billingWCU).build());
        }

        CreateTableRequest.Builder requestBuilder = CreateTableRequest.builder()
                .attributeDefinitions(
                        AttributeDefinition.builder().attributeName(FEATURE_UID).attributeType(ScalarAttributeType.S).build(),
                        AttributeDefinition.builder().attributeName(FEATURE_GROUP).attributeType(ScalarAttributeType.S).build()
                )
                .keySchema(KeySchemaElement.builder().attributeName(FEATURE_UID).keyType(KeyType.HASH).build())
                .globalSecondaryIndexes(globalSecondaryIndexBuilder.build())
                .tableName(tableName);

        requestBuilder.billingMode(billingMode);
        if (BillingMode.PROVISIONED.equals(billingMode)) {
            requestBuilder.provisionedThroughput(ProvisionedThroughput.builder().readCapacityUnits(billingRCU).writeCapacityUnits(billingWCU).build());
        }

        try {
            client.createTable(requestBuilder.build());
            client.waiter().waitUntilTableExists(builder -> builder.tableName(tableName));
            table = dynamoDB.table(tableName, TableSchema.fromClass(DynamoDbFeature.class));
        } catch (Exception e) {
            throw new IllegalStateException("Cannot initialize Feature Table in DynamoDB", e);
        }
    }

    /**
     * For test purpose, delete + recreate table instead (much more efficient, but slower for tests)
     */
    void clearTable() {
        PageIterable<DynamoDbFeature> scan = table.scan();

        if (scan != null && scan.iterator().hasNext()) {
            WriteBatch.Builder<DynamoDbFeature> builder = WriteBatch.builder(DynamoDbFeature.class)
                    .mappedTableResource(table);

            AtomicInteger count = new AtomicInteger();
            scan.forEach(dynamoDbFeaturePage ->
                    dynamoDbFeaturePage.items().forEach(dynamoDbFeature -> {
                                builder.addDeleteItem(Key.builder().partitionValue(dynamoDbFeature.getFeatureUid()).build());
                                count.getAndIncrement();
                            }
                    )
            );

            if (count.intValue() > 0) {
                dynamoDB.batchWriteItem(r -> r.writeBatches(
                        builder.build()
                ));
            }
        }
    }


}
