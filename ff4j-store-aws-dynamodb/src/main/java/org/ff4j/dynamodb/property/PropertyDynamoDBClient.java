package org.ff4j.dynamodb.property;

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
import com.amazonaws.services.dynamodbv2.document.spec.ScanSpec;
import com.amazonaws.services.dynamodbv2.model.*;
import org.ff4j.dynamodb.DynamoDBClient;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.ff4j.dynamodb.DynamoDBConstants.PROPERTY_NAME;
import static org.ff4j.dynamodb.DynamoDBConstants.PROPERTY_VALUE;

/**
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
class PropertyDynamoDBClient extends DynamoDBClient<Property<?>> {

    private final PropertyDynamoDBMapper PROPERTY_MAPPER = new PropertyDynamoDBMapper();

    PropertyDynamoDBClient(AmazonDynamoDB amazonDynamoDB, String tableName) {
        super(amazonDynamoDB, tableName);
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
        Item item = getItem(name);
        return PROPERTY_MAPPER.fromStore(item);
    }

    @Override
    protected Map<String, Property<?>> getAll() {
        ItemCollection<ScanOutcome> items = table.scan(new ScanSpec().withSelect(Select.ALL_ATTRIBUTES));
        Map<String, Property<?>> map = new HashMap<String, Property<?>>();

        for (Item item : items) {
            map.put(item.getString(PROPERTY_NAME), PROPERTY_MAPPER.fromStore(item));
        }

        return map;
    }


    void updateProperty(String propName, String newValue) {
        table.updateItem(new PrimaryKey(PROPERTY_NAME, propName), new AttributeUpdate(PROPERTY_VALUE).put(newValue));
    }

    Set<String> getAllNames() {
        ItemCollection<ScanOutcome> items = table.scan(new ScanSpec().withAttributesToGet(key));

        Set<String> names = new HashSet<String>();
        for (Item item : items) {
            names.add(item.getString(key));
        }
        return names;
    }

    @Override
    protected void createTable() {
        CreateTableRequest request = new CreateTableRequest()
                .withAttributeDefinitions(
                        new AttributeDefinition(PROPERTY_NAME, ScalarAttributeType.S)
                )
                .withKeySchema(new KeySchemaElement(PROPERTY_NAME, KeyType.HASH))
                .withProvisionedThroughput(new ProvisionedThroughput(5L, 5L))
                .withTableName(tableName);

        try {
            table = dynamoDB.createTable(request);
            table.waitForActive();
        } catch (Exception e) {
            throw new IllegalStateException("Cannot initialize Property Table in DynamoDB", e);
        }
    }

}
