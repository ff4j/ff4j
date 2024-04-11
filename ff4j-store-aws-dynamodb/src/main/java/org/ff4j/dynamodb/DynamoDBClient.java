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

import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbEnhancedClient;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbTable;
import software.amazon.awssdk.enhanced.dynamodb.Key;
import software.amazon.awssdk.enhanced.dynamodb.TableSchema;
import software.amazon.awssdk.services.dynamodb.DynamoDbClient;
import software.amazon.awssdk.services.dynamodb.model.BillingMode;
import software.amazon.awssdk.services.dynamodb.model.ResourceNotFoundException;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.ParameterizedType;
import java.util.Map;
import java.util.Properties;

import static org.ff4j.dynamodb.DynamoDBConstants.DEFAULT_RCU;
import static org.ff4j.dynamodb.DynamoDBConstants.DEFAULT_WCU;

/**
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public abstract class DynamoDBClient<FF4J, STORE> {

    private static final Logger LOGGER = LoggerFactory.getLogger(DynamoDBClient.class);

    protected final DynamoDbEnhancedClient dynamoDB;
    protected final DynamoDbClient client;
    private final Class<STORE> persistentClass;

    protected DynamoDbTable<STORE> table;
    protected String tableName;
    protected String key;
    protected BillingMode billingMode = BillingMode.PROVISIONED;
    protected Long billingRCU = DEFAULT_RCU;
    protected Long billingWCU = DEFAULT_WCU;

    public DynamoDBClient(DynamoDbClient client) {
        this.client = client;
        dynamoDB = DynamoDbEnhancedClient.builder().dynamoDbClient(client).build();
        loadPropertiesIfExist();
        this.persistentClass = (Class<STORE>) ((ParameterizedType) getClass()
                .getGenericSuperclass()).getActualTypeArguments()[1];
        table = dynamoDB.table(tableName, TableSchema.fromClass(persistentClass));
    }

    /**
     * Extension Point for DynamoDB client custom configuration.
     *
     * @param client
     *      dynamo client (from sdk)
     * @param props
     *      properties to initialize the component
     * @param tableName
     *      (optional) override default table name
     */
    public DynamoDBClient(DynamoDbClient client, Properties props, String tableName) {
        this.client          = client;
        this.dynamoDB        = DynamoDbEnhancedClient.builder().dynamoDbClient(client).build();
        this.persistentClass = (Class<STORE>) ((ParameterizedType) getClass()
                .getGenericSuperclass()).getActualTypeArguments()[1];
        // Load properties not from the classpath
        loadProperties(props);
        // Possibility to override tableName
        if (tableName != null) {
            this.tableName = tableName;
        }
        table = dynamoDB.table(tableName, TableSchema.fromClass(persistentClass));
    }

    protected abstract void createTable();
    protected abstract RuntimeException notFoundException(String id);
    protected abstract FF4J get(String id);
    protected abstract void put(FF4J t);
    protected abstract Map<String, FF4J> getAll();
    protected abstract void loadProperties(Properties prop);

    public STORE getItem(String id) {
        Util.assertHasLength(id);

        Key key = Key.builder().partitionValue(id).build();
        STORE item = table.getItem(r -> r.key(key));
        if (item == null) {
            throw notFoundException(id);
        }
        return item;
    }

    public void deleteItem(String id) {
        table.deleteItem(Key.builder().partitionValue(id).build());
    }


    private void loadPropertiesIfExist() {
        InputStream in = this.getClass().getClassLoader().getResourceAsStream(DynamoDBConstants.CONFIG_FILE);
        if (in != null) {
            Properties prop = new Properties();
            try {
                prop.load(in);
            } catch (IOException e) {
                e.printStackTrace();
            }
            loadProperties(prop);
        } else {
            throw new RuntimeException("ff4j-dynamodb.properties was not found in the classpath to setup the DynamoDB store");
        }
    }

    public boolean tableExists() {
        try {
            client.describeTable(builder -> builder.tableName(tableName).build());
            table = dynamoDB.table(tableName, TableSchema.fromClass(persistentClass));
        } catch (ResourceNotFoundException e) {
            return false;
        }
        return true;
    }

    public void deleteTable() {
        client.deleteTable(builder -> builder.tableName(tableName).build());
        client.waiter().waitUntilTableNotExists(builder -> builder.tableName(tableName));
    }

}
