package org.ff4j.dynamodb.property;

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

class PropertyDynamoDBClient extends DynamoDBClient {

    private final PropertyDynamoDBMapper PROPERTY_MAPPER = new PropertyDynamoDBMapper();

    PropertyDynamoDBClient(AmazonDynamoDB amazonDynamoDB, String tableName) {
        super(amazonDynamoDB, tableName);
        key = PROPERTY_NAME;
    }

    @Override
    protected RuntimeException notFoundException(String id) {
        return new PropertyNotFoundException(id);
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

    Map<String, Property<?>> getAllProperties() {
        ItemCollection<ScanOutcome> items = table.scan(new ScanSpec().withSelect(Select.ALL_ATTRIBUTES));
        Map<String, Property<?>> map = new HashMap<String, Property<?>>();

        for (Item item : items) {
            map.put(item.getString(PROPERTY_NAME), PROPERTY_MAPPER.fromStore(item));
        }

        return map;
    }

    Set<String> getAllNames() {
        ItemCollection<ScanOutcome> items = table.scan(new ScanSpec().withAttributesToGet(key));

        Set<String> names = new HashSet<String>();
        for (Item item : items) {
            names.add(item.getString(key));
        }
        return names;
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

}
