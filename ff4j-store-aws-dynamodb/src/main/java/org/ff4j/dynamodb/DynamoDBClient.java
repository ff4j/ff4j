package org.ff4j.dynamodb;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.document.*;
import com.amazonaws.services.dynamodbv2.document.spec.GetItemSpec;
import com.amazonaws.services.dynamodbv2.model.BillingMode;
import com.amazonaws.services.dynamodbv2.model.ResourceNotFoundException;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Properties;

import static org.ff4j.dynamodb.DynamoDBConstants.DEFAULT_RCU;
import static org.ff4j.dynamodb.DynamoDBConstants.DEFAULT_WCU;

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

/**
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public abstract class DynamoDBClient<T> {

    private static final Logger LOGGER = LoggerFactory.getLogger(DynamoDBClient.class);


    private final AmazonDynamoDB amazonDynamoDB;
    protected final DynamoDB dynamoDB;
    protected String tableName;
    protected String key;
    protected Table table;
    protected BillingMode billingMode = BillingMode.PROVISIONED;
    protected Long billingRCU = DEFAULT_RCU;
    protected Long billingWCU = DEFAULT_WCU;

    /**
     * @deprecated table name will soon be removed from the constructor, use the ff4j-dynamodb.properties file instead
     */
    @Deprecated
    public DynamoDBClient(AmazonDynamoDB amazonDynamoDB, String tableName) {
        this.amazonDynamoDB = amazonDynamoDB;
        this.dynamoDB = new DynamoDB(amazonDynamoDB);
        this.tableName = tableName;
        loadPropertiesIfExist();
        this.table = dynamoDB.getTable(this.tableName);
    }

    protected abstract void createTable();
    protected abstract RuntimeException notFoundException(String id);
    protected abstract T get(String id);
    protected abstract void put(T t);
    protected abstract Map<String, T> getAll();
    protected abstract void loadProperties(Properties prop);

    public void deleteItem(String id) {
        table.deleteItem(new KeyAttribute(key, id));
    }

    public Item getItem(String id) {
        Util.assertHasLength(id);

        Item item = table.getItem(new GetItemSpec().withPrimaryKey(new PrimaryKey(key, id)));
        if (item == null) {
            throw notFoundException(id);
        }
        return item;
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
            LOGGER.warn("You should consider using ff4j-dynamodb.properties to setup the DynamoDB store");
        }
    }

    public boolean tableExists() {
        try {
            amazonDynamoDB.describeTable(tableName);
            table = dynamoDB.getTable(tableName);
        } catch (ResourceNotFoundException e) {
            return false;
        }
        return true;
    }


    public void deleteTable() {
        table.delete();
        try {
            table.waitForDelete();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

}
