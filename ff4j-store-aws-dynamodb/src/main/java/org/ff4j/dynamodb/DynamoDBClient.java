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
import com.amazonaws.services.dynamodbv2.document.spec.ScanSpec;
import com.amazonaws.services.dynamodbv2.model.*;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.utils.Util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.ff4j.dynamodb.DynamoDBConstants.*;

/**
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
class DynamoDBClient {

    private final DynamoDBPropertyMapper MAPPER = new DynamoDBPropertyMapper();

    private final AmazonDynamoDB amazonDynamoDB;
    private final DynamoDB dynamoDB;
    private String tableName;
    private Table table;

    DynamoDBClient(AmazonDynamoDB amazonDynamoDB, String tableName) {
        this.amazonDynamoDB = amazonDynamoDB;
        this.dynamoDB = new DynamoDB(amazonDynamoDB);
        this.tableName = tableName;
        this.table = dynamoDB.getTable(tableName);
    }

    void putProperty(Property property) {
        table.putItem(MAPPER.toStore(property));
    }

    void updateProperty(String propName, String newValue) {
        table.updateItem(new PrimaryKey(PROPERTY_NAME, propName), new AttributeUpdate(PROPERTY_VALUE).put(newValue));
    }

    Property getProperty(String name) {
        Item item = getItem(name);
        return MAPPER.fromStore(item);
    }

    void deleteProperty(String name) {
        table.deleteItem(new KeyAttribute(PROPERTY_NAME, name));
    }

    Map<String, Property<?>> getAllProperties() {
        ItemCollection<ScanOutcome> items = table.scan(new ScanSpec().withSelect(Select.ALL_ATTRIBUTES));
        Map<String, Property<?>> map = new HashMap<String, Property<?>>();

        for (Item item : items) {
            map.put(item.getString(PROPERTY_NAME), MAPPER.fromStore(item));
        }

        return map;
    }

    Set<String> getAllPropertyNames() {
        ItemCollection<ScanOutcome> items = table.scan(new ScanSpec().withAttributesToGet(PROPERTY_NAME));

        Set<String> names = new HashSet<String>();
        for (Item item : items) {
            names.add(item.getString(PROPERTY_NAME));
        }
        return names;
    }

    Item getItem(String name) {
        Util.assertHasLength(name);

        Item item = table.getItem(new GetItemSpec().withPrimaryKey(new PrimaryKey(PROPERTY_NAME, name)));
        if (item == null) {
            throw new PropertyNotFoundException(name);
        }
        return item;
    }

    void createTable() {
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
