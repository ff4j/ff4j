package org.ff4j.dynamodb.property;

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

import org.ff4j.dynamodb.DynamoDBClient;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import software.amazon.awssdk.enhanced.dynamodb.TableSchema;
import software.amazon.awssdk.enhanced.dynamodb.model.PageIterable;
import software.amazon.awssdk.services.dynamodb.DynamoDbClient;
import software.amazon.awssdk.services.dynamodb.model.*;

import java.util.*;

import static org.ff4j.dynamodb.DynamoDBConstants.*;


/**
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
class PropertyDynamoDBClient extends DynamoDBClient<Property<?>, DynamoDbProperty> {

    private final PropertyDynamoDBMapper PROPERTY_MAPPER = new PropertyDynamoDBMapper();

    PropertyDynamoDBClient(DynamoDbClient client) {
        super(client);
        key = PROPERTY_NAME;
    }

    @Override
    protected RuntimeException notFoundException(String id) {
        return new PropertyNotFoundException(id);
    }

    @Override
    protected void put(Property<?> property) {
        table.putItem(PROPERTY_MAPPER.toStore(property));
    }

    @Override
    protected Property<?> get(String name) {
        DynamoDbProperty item = getItem(name);
        return PROPERTY_MAPPER.fromStore(item);
    }

    @Override
    protected Map<String, Property<?>> getAll() {
        PageIterable<DynamoDbProperty> pages = table.scan();

        Map<String, Property<?>> map = new HashMap<>();
        pages.items().forEach(dynamoDbProperty -> map.put(dynamoDbProperty.getName(), PROPERTY_MAPPER.fromStore(dynamoDbProperty)));

        return map;
    }

    void updateProperty(String propName, String newValue) {
        DynamoDbProperty item = getItem(propName);
        if (item.getValue().equals(newValue)) {
            return;
        }
        item.setValue(newValue);
        table.updateItem(item);
    }

    Set<String> getAllNames() {
        PageIterable<DynamoDbProperty> pages = table.scan(builder -> builder.attributesToProject(key));

        Set<String> propNames = new HashSet<>();
        pages.items().forEach(dynamoDbFeature -> propNames.add(dynamoDbFeature.getName()));

        return propNames;
    }

    @Override
    protected void loadProperties(Properties prop) {
        tableName = prop.getProperty(CONFIG_PROPERTY_TABLE_NAME, PROPERTY_TABLE_NAME);

        String billing = prop.getProperty(CONFIG_PROPERTY_BILLING, BillingMode.PROVISIONED.toString());
        this.billingMode = BillingMode.valueOf(billing);

        String rcu = prop.getProperty(CONFIG_PROPERTY_RCU, String.valueOf(DEFAULT_RCU));
        this.billingRCU = Long.valueOf(rcu);

        String wcu = prop.getProperty(CONFIG_PROPERTY_WCU, String.valueOf(DEFAULT_WCU));
        this.billingWCU = Long.valueOf(wcu);
    }

    @Override
    protected void createTable() {
        CreateTableRequest.Builder requestBuilder = CreateTableRequest.builder()
                .attributeDefinitions(
                        AttributeDefinition.builder().attributeName(PROPERTY_NAME).attributeType(ScalarAttributeType.S).build()
                )
                .keySchema(KeySchemaElement.builder().attributeName(PROPERTY_NAME).keyType(KeyType.HASH).build())
                .tableName(tableName);

        requestBuilder.billingMode(billingMode);
        if (BillingMode.PROVISIONED.equals(billingMode)) {
            requestBuilder.provisionedThroughput(ProvisionedThroughput.builder().readCapacityUnits(billingRCU).writeCapacityUnits(billingWCU).build());
        }

        try {
            client.createTable(requestBuilder.build());
            client.waiter().waitUntilTableExists(builder -> builder.tableName(tableName));
            table = dynamoDB.table(tableName, TableSchema.fromClass(DynamoDbProperty.class));
        } catch (Exception e) {
            throw new IllegalStateException("Cannot initialize Property Table in DynamoDB", e);
        }
    }
}
