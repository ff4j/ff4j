package org.ff4j.dynamodb.property;

import static org.ff4j.dynamodb.DynamoDBConstants.CONFIG_PROPERTY_BILLING;
import static org.ff4j.dynamodb.DynamoDBConstants.CONFIG_PROPERTY_RCU;
import static org.ff4j.dynamodb.DynamoDBConstants.CONFIG_PROPERTY_TABLE_NAME;
import static org.ff4j.dynamodb.DynamoDBConstants.CONFIG_PROPERTY_WCU;
import static org.ff4j.dynamodb.DynamoDBConstants.DEFAULT_RCU;
import static org.ff4j.dynamodb.DynamoDBConstants.DEFAULT_WCU;
import static org.ff4j.dynamodb.DynamoDBConstants.PROPERTY_NAME;
import static org.ff4j.dynamodb.DynamoDBConstants.PROPERTY_TABLE_NAME;
import static org.ff4j.dynamodb.DynamoDBConstants.PROPERTY_VALUE;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.ff4j.dynamodb.DynamoDBClient;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;

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
import com.amazonaws.services.dynamodbv2.document.AttributeUpdate;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.document.ItemCollection;
import com.amazonaws.services.dynamodbv2.document.PrimaryKey;
import com.amazonaws.services.dynamodbv2.document.ScanOutcome;
import com.amazonaws.services.dynamodbv2.document.spec.ScanSpec;
import com.amazonaws.services.dynamodbv2.model.AttributeDefinition;
import com.amazonaws.services.dynamodbv2.model.BillingMode;
import com.amazonaws.services.dynamodbv2.model.CreateTableRequest;
import com.amazonaws.services.dynamodbv2.model.KeySchemaElement;
import com.amazonaws.services.dynamodbv2.model.KeyType;
import com.amazonaws.services.dynamodbv2.model.ProvisionedThroughput;
import com.amazonaws.services.dynamodbv2.model.ScalarAttributeType;
import com.amazonaws.services.dynamodbv2.model.Select;

/**
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
class PropertyDynamoDBClient extends DynamoDBClient<Property<?>> {

    private final PropertyDynamoDBMapper PROPERTY_MAPPER = new PropertyDynamoDBMapper();

    /**
     * @deprecated table name will soon be removed from the constructor, use the ff4j-dynamodb.properties file instead
     */
    @Deprecated
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
        CreateTableRequest request = new CreateTableRequest()
                .withAttributeDefinitions(
                        new AttributeDefinition(PROPERTY_NAME, ScalarAttributeType.S)
                )
                .withKeySchema(new KeySchemaElement(PROPERTY_NAME, KeyType.HASH))
                .withTableName(tableName);

        request.setBillingMode(billingMode.toString());
        if (billingMode.equals(BillingMode.PROVISIONED)) {
            request.setProvisionedThroughput(new ProvisionedThroughput(billingRCU, billingWCU));
        }

        try {
            table = dynamoDB.createTable(request);
            table.waitForActive();
        } catch (Exception e) {
            throw new IllegalStateException("Cannot initialize Property Table in DynamoDB", e);
        }
    }

}
