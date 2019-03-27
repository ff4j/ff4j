package org.ff4j.dynamodb.feature;

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
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder;
import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Set;

import static org.ff4j.dynamodb.DynamoDBConstants.FEATURE_TABLE_NAME;

/**
 * Implementation of {@link org.ff4j.core.FeatureStore} using Amazon DynamoDB.<br />
 * <p>
 * To get it running, a DynamoDB table is required.
 * Either you let FF4J create it for you with default configuration (ProvisionedThroughput: 5RCU & 5WCU) or
 * you create it on your own. You must keep the same attribute names, but you can change the table name, index name,
 * billing mode and throughput. Example:<br/>
 * <code>
 * aws dynamodb create-table --cli-input-json file://create-feature-dynamodb-table.json
 * </code>
 * <p>
 * With the following content in create-feature-dynamodb-table.json file:
 * <code>
 * {
 * "TableName": "ff4jfeatures",
 * "AttributeDefinitions": [
 * {
 * "AttributeName": "featureUid",
 * "AttributeType": "S"
 * },
 * {
 * "AttributeName": "groupName",
 * "AttributeType": "S"
 * }
 * ],
 * "KeySchema": [
 * {
 * "AttributeName": "featureUid",
 * "KeyType": "HASH"
 * }
 * ],
 * "GlobalSecondaryIndexes": [
 * {
 * "IndexName": "ff4jfeaturesgroup",
 * "KeySchema": [
 * {
 * "AttributeName": "groupName",
 * "KeyType": "HASH"
 * }
 * ],
 * "Projection": {
 * "ProjectionType": "ALL"
 * },
 * "ProvisionedThroughput": {
 * "ReadCapacityUnits": 4,
 * "WriteCapacityUnits": 4
 * }
 * }
 * ],
 * "BillingMode": "PROVISIONED",
 * "ProvisionedThroughput": {
 * "ReadCapacityUnits": 4,
 * "WriteCapacityUnits": 4
 * }
 * }
 * </code>
 * </p>
 * <p>If you change the table name, use the appropriate constructor, passing the table name as parameter: <ul>
 * <li>{@link #FeatureStoreDynamoDB(String)}</li>
 * <li>{@link #FeatureStoreDynamoDB(AmazonDynamoDB, String)}</li>
 * </ul></p>
 * <p>If you want to get more control on the connection to Amazon DynamoDB, use the appropriate constructor:<ul>
 * <li>{@link #FeatureStoreDynamoDB(AmazonDynamoDB)}</li>
 * <li>{@link #FeatureStoreDynamoDB(AmazonDynamoDB, String)}</li>
 * </ul></p>
 *
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public class FeatureStoreDynamoDB extends AbstractFeatureStore {

    private static final Logger LOGGER = LoggerFactory.getLogger(FeatureStoreDynamoDB.class);

    /**
     * Internal DynamoDB client
     */
    private FeatureDynamoDBClient dynamoDBClient;

    /************************************************************************************************************/
    /**                                           CONSTRUCTORS                                                  */
    /************************************************************************************************************/

    /**
     * Default constructor using default DynamoDB client and default table name.
     * If you need more control on AWS connection (credentials, proxy, ...), use {@link #FeatureStoreDynamoDB(AmazonDynamoDB)}
     */
    public FeatureStoreDynamoDB() {
        this(AmazonDynamoDBClientBuilder.defaultClient(), FEATURE_TABLE_NAME);
    }

    /**
     * Constructor using default DynamoDB client and custom table name.
     * If you need more control on AWS connection (credentials, proxy, ...), use {@link #FeatureStoreDynamoDB(AmazonDynamoDB, String)}
     *
     * @param tableName name of the table to use in DynamoDB
     */
    public FeatureStoreDynamoDB(String tableName) {
        this(AmazonDynamoDBClientBuilder.defaultClient(), tableName);
    }

    /**
     * Constructor using custom DynamoDB client and default table name.
     *
     * @param amazonDynamoDB Amazon DynamoDB client
     */
    public FeatureStoreDynamoDB(AmazonDynamoDB amazonDynamoDB) {
        this(amazonDynamoDB, FEATURE_TABLE_NAME);
    }

    /**
     * Constructor using custom DynamoDB client and table name.
     *
     * @param amazonDynamoDB Amazon DynamoDB client
     * @param tableName      name of the table to use in DynamoDB
     */
    public FeatureStoreDynamoDB(AmazonDynamoDB amazonDynamoDB, String tableName) {
        initStore(amazonDynamoDB, tableName);
    }

    /************************************************************************************************************/
    /**                                              PUBLIC                                                     */
    /************************************************************************************************************/

    /*************/
    /** FEATURES */
    /*************/

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean exist(String featId) {
        try {
            getClient().getItem(featId);
        } catch (FeatureNotFoundException e) {
            return false;
        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void create(Feature feature) {
        Util.assertNotNull(feature);
        assertFeatureNotExist(feature.getUid());

        getClient().put(feature);
        LOGGER.info("Feature " + feature.getUid() + " created");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void enable(String uid) {
        assertFeatureExist(uid);
        getClient().updateFeatureAvailability(uid, true);
        LOGGER.info("Feature " + uid + " enabled");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disable(String uid) {
        assertFeatureExist(uid);
        getClient().updateFeatureAvailability(uid, false);
        LOGGER.info("Feature " + uid + " disabled");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Feature read(String featureUid) {
        return getClient().get(featureUid);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Feature> readAll() {
        return getClient().getAll();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void delete(String featureId) {
        assertFeatureExist(featureId);

        getClient().deleteItem(featureId);
        LOGGER.info("Feature " + featureId + " deleted");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(Feature feature) {
        Util.assertNotNull(feature);

//        getClient().update(feature);
        delete(feature.getUid());
        create(feature);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear() {
        deleteTable();
        createSchema();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createSchema() {
        if (!getClient().tableExists()) {
            getClient().createTable();
            LOGGER.info("Feature table created");
        }
    }


    /***********/
    /** GROUPS */
    /***********/

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertHasLength(groupName);
        try {
            getClient().getItemsByGroup(groupName);
        } catch (GroupNotFoundException ge) {
            return false;
        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        Util.assertHasLength(groupName);

        return getClient().getFeaturesByGroup(groupName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> readAllGroups() {
        return getClient().getAllGroups();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addToGroup(String featureId, String groupName) {
        assertFeatureExist(featureId);
        Util.assertHasLength(groupName);

        getClient().addToGroup(featureId, groupName);
        LOGGER.info("Group " + groupName + " added to feature " + featureId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
        assertFeatureExist(featureId);
        assertGroupExist(groupName);

        getClient().removeFromGroup(featureId);
        LOGGER.info("Group " + groupName + " removed from feature " + featureId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void enableGroup(String groupName) {
        Util.assertHasLength(groupName);
        getClient().updateFeatureAvailabilityInGroup(groupName, true);
        LOGGER.info("Group " + groupName + " enabled");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disableGroup(String groupName) {
        Util.assertHasLength(groupName);
        getClient().updateFeatureAvailabilityInGroup(groupName, false);
        LOGGER.info("Group " + groupName + " disabled");
    }

    /****************/
    /** PERMISSIONS */
    /****************/

    /**
     * {@inheritDoc}
     */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        Util.assertHasLength(roleName);
        getClient().addFeaturePermission(flipId, roleName);
        LOGGER.info("Role " + roleName + " granted on feature "+ flipId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        Util.assertHasLength(roleName);
        getClient().removeFeaturePermission(flipId, roleName);
        LOGGER.info("Role " + roleName + " revoked on feature "+ flipId);
    }

    /************************************************************************************************************/
    /**                                              PRIVATE                                                    */
    /************************************************************************************************************/

    /**
     * Initialize internal dynamoDB client and create DynamoDB table if necessary
     *
     * @param amazonDynamoDB dynamoDB client
     * @param tableName      name of the table in DynamoDB
     */
    private void initStore(AmazonDynamoDB amazonDynamoDB, String tableName) {
        dynamoDBClient = new FeatureDynamoDBClient(amazonDynamoDB, tableName);

        createSchema();
    }

    /**
     * Getter accessor for attribute 'dynamoDBClient'.
     *
     * @return current value of 'dynamoDBClient'
     */
    private FeatureDynamoDBClient getClient() {
        return dynamoDBClient;
    }

    private void deleteTable() {
        getClient().deleteTable();
        LOGGER.info("Feature table deleted");
    }

    /**
     * For test purpose only
     * Delete + recreate table instead, see {@link #clear} : much more efficient, but slower for tests
     */
    void clearData() {
        getClient().clearTable();
    }
}
