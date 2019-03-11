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

import java.util.Map;
import java.util.Set;

import static org.ff4j.dynamodb.DynamoDBConstants.FEATURE_TABLE_NAME;

/**
 *  Implementation of {@link org.ff4j.core.FeatureStore} using Amazon DynamoDB.
 *
 *  @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public class FeatureStoreDynamoDB extends AbstractFeatureStore {

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
     * @param tableName name of the table to use in DynamoDB
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

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        try {
            getClient().getItem(featId);
        } catch (FeatureNotFoundException e) {
            return false;
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature feature) {
        Util.assertNotNull(feature);
        assertFeatureNotExist(feature.getUid());

        getClient().putFeature(feature);
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        assertFeatureExist(uid);
        getClient().updateFeatureAvailability(uid, true);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        assertFeatureExist(uid);
        getClient().updateFeatureAvailability(uid, false);
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featureUid) {
        return getClient().getFeature(featureUid);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        return getClient().getAllFeatures();
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String featureId) {
        assertFeatureExist(featureId);

        getClient().deleteItem(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature feature) {
        Util.assertNotNull(feature);
        assertFeatureExist(feature.getUid());

        delete(feature.getUid());
        create(feature);
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        getClient().deleteTable();
        createSchema();
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        getClient().createFeatureTable();
    }

    /***********/
    /** GROUPS */
    /***********/

    /** {@inheritDoc} */
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

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        Util.assertHasLength(groupName);
        return getClient().getFeaturesByGroup(groupName);
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        return getClient().getAllGroups();
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String featureId, String groupName) {
        assertFeatureExist(featureId);
        Util.assertHasLength(groupName);

        getClient().addToGroup(featureId, groupName);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
        assertFeatureExist(featureId);
        assertGroupExist(groupName);

        // FIXME : verify feature has this group
        getClient().removeFromGroup(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        Util.assertHasLength(groupName);
        getClient().updateFeatureAvailabilityInGroup(groupName, true);
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        Util.assertHasLength(groupName);
        getClient().updateFeatureAvailabilityInGroup(groupName, false);
    }

    /****************/
    /** PERMISSIONS */
    /****************/

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        super.grantRoleOnFeature(flipId, roleName);
        // TODO
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        super.removeRoleFromFeature(flipId, roleName);
        // TODO
    }

    /************************************************************************************************************/
    /**                                              PRIVATE                                                    */
    /************************************************************************************************************/

    /**
     * Initialize internal dynamoDB client and create DynamoDB table if necessary
     *
     * @param amazonDynamoDB dynamoDB client
     * @param tableName name of the table in DynamoDB
     */
    private void initStore(AmazonDynamoDB amazonDynamoDB, String tableName) {
        dynamoDBClient = new FeatureDynamoDBClient(amazonDynamoDB, tableName);

        if (!getClient().tableExists()) {
            createSchema();
        }
    }

    /**
     * Getter accessor for attribute 'dynamoDBClient'.
     *
     * @return
     *       current value of 'dynamoDBClient'
     */
    private FeatureDynamoDBClient getClient() {
        return dynamoDBClient;
    }
}
