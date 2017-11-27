package org.ff4j.couchbase;

/*
 * #%L
 * ff4j-store-couchbase
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import static org.ff4j.couchbase.CouchbaseConstants.DEFAULT_AUDIT_BUCKETNAME;
import static org.ff4j.couchbase.CouchbaseConstants.DEFAULT_AUDIT_VIEWTNAME;
import static org.ff4j.couchbase.CouchbaseConstants.DEFAULT_FEATURE_BUCKETNAME;
import static org.ff4j.couchbase.CouchbaseConstants.DEFAULT_PROPERTY_BUCKETNAME;

import java.util.HashSet;
import java.util.Set;

import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.couchbase.client.java.Bucket;
import com.couchbase.client.java.Cluster;
import com.couchbase.client.java.CouchbaseCluster;
import com.couchbase.client.java.env.CouchbaseEnvironment;

/**
 * Wrapper to handle connectivity to CouchBase.
 *
 * @author farrellyja
 * @author Cedrick LUNVEN (@clunven)
 */
public class CouchbaseConnection {

    /** logger for this class. */
    private static final Logger LOGGER = LoggerFactory.getLogger(CouchbaseConnection.class);
    
    /** CouchBase cluster. */
    private Cluster couchBaseCluster;
    
    /** UserName. */
    private String userName = null;
    
    /** Password. */
    private String password = null;
    
    /** Override the default parameters. */
    private CouchbaseEnvironment couchBaseEnvironment;
    
    /** Connection  String. */
    private String connectionString;
    
    /** ClusterNodes. */
    private Set <String> clusterNodes = new HashSet<>();
    
    /** Data containers used in Couchbase. */
    private Bucket ff4jFeatureBucket;
    
    /** Feature Bucket. */
    private String ff4jFeatureBucketName = DEFAULT_FEATURE_BUCKETNAME;
    
    /**  Name of the Bucket. */
    private String ff4jFeatureBucketPassword = "";
    
    /** Data containers used in Couchbase. */
    private Bucket ff4jPropertyBucket;
    
    /**  Name of the Bucket. */
    private String ff4jPropertyBucketName = DEFAULT_PROPERTY_BUCKETNAME;
    
    /**  Name of the Bucket. */
    private String ff4jPropertyBucketPassword = "";
    
    /** Data containers used in Couchbase. */
    private Bucket ff4jAuditBucket;
    
    /**  Name of the Bucket. */
    private String ff4jAuditBucketName = DEFAULT_AUDIT_BUCKETNAME;
    
    /**  Name of the Bucket. */
    private String ff4jAuditViewName = DEFAULT_AUDIT_VIEWTNAME;
    
    /**  Name of the Bucket. */
    private String ff4jAuditBucketPassword = "";
    
    /** Default. */
    public CouchbaseConnection() {}
    
    /** Default. */
    public CouchbaseConnection(String name) {
        this.connectionString = name;
    }
    
    /**
     * Fluent method to init connectivity.
     */
    public CouchbaseConnection nodes(Set < String> nodes) {
        this.clusterNodes = nodes;
        return this;
    }
    
    /**
     * Fluent method to init connectivity.
     */
    public CouchbaseConnection featureBucketName(String bucketName) {
        this.ff4jFeatureBucketName = bucketName;
        return this;
    }
    
    /**
     * Fluent method to init connectivity.
     */
    public CouchbaseConnection propertyBucketName(String bucketName) {
        this.ff4jPropertyBucketName = bucketName;
        return this;
    }
    
    /**
     * Fluent method to init connectivity.
     */
    public CouchbaseConnection auditBucketName(String bucketName) {
        this.ff4jAuditBucketName = bucketName;
        return this;
    }
    
    /**
     * Fluent method to init connectivity.
     */
    public CouchbaseConnection featureBucketPassword(String bucketPassword) {
        this.ff4jFeatureBucketPassword = bucketPassword;
        return this;
    }
    
    /**
     * Fluent method to init connectivity.
     */
    public CouchbaseConnection propertyBucketPassword(String bucketPassword) {
        this.ff4jPropertyBucketPassword = bucketPassword;
        return this;
    }
    
    /**
     * Fluent method to init connectivity.
     */
    public CouchbaseConnection auditBucketPassword(String bucketPassword) {
        this.ff4jAuditBucketPassword = bucketPassword;
        return this;
    }
    
    /**
     * Fluent method to init connectivity.
     */
    public CouchbaseConnection auditView(String viewName) {
        this.ff4jAuditViewName = viewName;
        return this;
    }
    
    /**
     * Fluent method to init connectivity.
     */
    public CouchbaseConnection couchBaseEnvironment(CouchbaseEnvironment env) {
        this.couchBaseEnvironment = env;
        return this;
    }
    
    /**
     * Fluent method to init connectivity.
     *
     * @param url
     *      target couchbase url.
     * @return
     *      current connection.
     */
    public CouchbaseConnection addNode(String node) {
        this.clusterNodes.add(node);
        return this;
    }
    
    /**
     * Initialization of the cluster.
     */
    public void initCluster() {
        if (couchBaseCluster == null) {
            // Initialization from STRING
            if (connectionString != null) {
                LOGGER.info("Initializing connectivity from ConnectionString...");
                if (couchBaseEnvironment != null) {
                    this.couchBaseCluster = CouchbaseCluster.fromConnectionString(couchBaseEnvironment, connectionString);
                } else {
                    this.couchBaseCluster = CouchbaseCluster.fromConnectionString(connectionString);
                }
            }
            // Initialization from NODES
            else if (!clusterNodes.isEmpty()) {
                LOGGER.info("Initializing connectivity from Nodes...");
                if (couchBaseEnvironment != null) {
                    this.couchBaseCluster = CouchbaseCluster.create(couchBaseEnvironment, clusterNodes.toArray(new String[0]));
                } else {
                    this.couchBaseCluster = CouchbaseCluster.create(clusterNodes.toArray(new String[0]));
                }
            } 
            // Default (and basic) and  initialization
            else {
                LOGGER.info("Initializing connectivity (Default conf)");
                if (couchBaseEnvironment != null) {
                    this.couchBaseCluster = CouchbaseCluster.create(couchBaseEnvironment);
                } else {
                    this.couchBaseCluster = CouchbaseCluster.create();
                }
            }
        }
        if (Util.hasLength(userName) && Util.hasLength(password)) {
            couchBaseCluster.authenticate(userName, password);
        }
        LOGGER.info("Connection Etablished " + couchBaseCluster.toString());
    }
    
    /**
     * Initiliazing Buckets.
     */
    public void initFeatureBuckets() {
        if (ff4jFeatureBucket == null) {
            if (Util.hasLength(ff4jFeatureBucketPassword)) {
                ff4jFeatureBucket = getCluster().openBucket(ff4jFeatureBucketName, ff4jFeatureBucketPassword);
            } else {
                ff4jFeatureBucket = getCluster().openBucket(ff4jFeatureBucketName);
            }
        }
    }
    
    /**
     * Initialize Property Bucket. If Features and properties have the same name, need to check.
     */
    public void initPropertyBucket() {
        if (ff4jPropertyBucket == null) {
            if (Util.hasLength(ff4jPropertyBucketPassword)) {
                ff4jPropertyBucket = getCluster().openBucket(ff4jPropertyBucketName, ff4jPropertyBucketPassword);
            } else {
                ff4jPropertyBucket = getCluster().openBucket(ff4jPropertyBucketName);
            }
        }
    }
    
    /**
     * Initialize Audit Bucket. If Features and properties have the same name, need to check.
     */
    
    public void initAuditBucket() {
        if (ff4jAuditBucket == null) {
            if (Util.hasLength(ff4jAuditBucketPassword)) {
                ff4jAuditBucket = getCluster().openBucket(ff4jAuditBucketName, ff4jAuditBucketPassword);
            } else {
                ff4jAuditBucket = getCluster().openBucket(ff4jAuditBucketName);
            }
        }
    }
    
    /**
     * Getter accessor for attribute 'cluster'.
     *
     * @return
     *       current value of 'cluster'
     */
    public Cluster getCluster() {
        initCluster();
        return couchBaseCluster;
    }
    
    /**
     * Retrieve bucket for features.
     *
     * @return
     *      target feature bucket
     */
    public Bucket getFeaturesBucket() {
        initFeatureBuckets();
        return ff4jFeatureBucket;
    }
    
    /**
     * Retrieve bucket for features.
     *
     * @return
     *      target feature bucket
     */
    public Bucket getPropertiesBucket() {
        initPropertyBucket();
        return ff4jPropertyBucket;
    }
    
    /**
     * Retrieve bucket for features.
     *
     * @return
     *      target feature bucket
     */
    public Bucket getAuditBucket() {
        initAuditBucket();
        return ff4jAuditBucket;
    }

    /**
     * Disconnect from cluster
     */
    public void disconnect() {
        getCluster().disconnect();
    }
    
    /**
     * Close current bucket
     */
    public void closeBuckets() {
        if (ff4jFeatureBucket != null) {
            ff4jFeatureBucket.close();
        }
        if (ff4jPropertyBucket != null) {
            ff4jPropertyBucket.close();
        }
        if (ff4jAuditBucket != null) {
            ff4jAuditBucket.close();
        }
    }

    /**
     * Getter accessor for attribute 'ff4jFeatureBucketName'.
     *
     * @return
     *       current value of 'ff4jFeatureBucketName'
     */
    public String getFf4jFeatureBucketName() {
        return ff4jFeatureBucketName;
    }

    /**
     * Setter accessor for attribute 'ff4jFeatureBucketName'.
     * @param ff4jFeatureBucketName
     * 		new value for 'ff4jFeatureBucketName '
     */
    public void setFf4jFeatureBucketName(String ff4jFeatureBucketName) {
        this.ff4jFeatureBucketName = ff4jFeatureBucketName;
    }

    /**
     * Getter accessor for attribute 'ff4jFeatureBucketPassword'.
     *
     * @return
     *       current value of 'ff4jFeatureBucketPassword'
     */
    public String getFf4jFeatureBucketPassword() {
        return ff4jFeatureBucketPassword;
    }

    /**
     * Setter accessor for attribute 'ff4jFeatureBucketPassword'.
     * @param ff4jFeatureBucketPassword
     * 		new value for 'ff4jFeatureBucketPassword '
     */
    public void setFf4jFeatureBucketPassword(String ff4jFeatureBucketPassword) {
        this.ff4jFeatureBucketPassword = ff4jFeatureBucketPassword;
    }

    /**
     * Getter accessor for attribute 'ff4jPropertyBucketName'.
     *
     * @return
     *       current value of 'ff4jPropertyBucketName'
     */
    public String getFf4jPropertyBucketName() {
        return ff4jPropertyBucketName;
    }

    /**
     * Setter accessor for attribute 'ff4jPropertyBucketName'.
     * @param ff4jPropertyBucketName
     * 		new value for 'ff4jPropertyBucketName '
     */
    public void setFf4jPropertyBucketName(String ff4jPropertyBucketName) {
        this.ff4jPropertyBucketName = ff4jPropertyBucketName;
    }

    /**
     * Getter accessor for attribute 'ff4jPropertyBucketPassword'.
     *
     * @return
     *       current value of 'ff4jPropertyBucketPassword'
     */
    public String getFf4jPropertyBucketPassword() {
        return ff4jPropertyBucketPassword;
    }

    /**
     * Setter accessor for attribute 'ff4jPropertyBucketPassword'.
     * @param ff4jPropertyBucketPassword
     * 		new value for 'ff4jPropertyBucketPassword '
     */
    public void setFf4jPropertyBucketPassword(String ff4jPropertyBucketPassword) {
        this.ff4jPropertyBucketPassword = ff4jPropertyBucketPassword;
    }

    /**
     * Getter accessor for attribute 'ff4jAuditBucketName'.
     *
     * @return
     *       current value of 'ff4jAuditBucketName'
     */
    public String getFf4jAuditBucketName() {
        return ff4jAuditBucketName;
    }

    /**
     * Setter accessor for attribute 'ff4jAuditBucketName'.
     * @param ff4jAuditBucketName
     * 		new value for 'ff4jAuditBucketName '
     */
    public void setFf4jAuditBucketName(String ff4jAuditBucketName) {
        this.ff4jAuditBucketName = ff4jAuditBucketName;
    }

    /**
     * Getter accessor for attribute 'ff4jAuditViewName'.
     *
     * @return
     *       current value of 'ff4jAuditViewName'
     */
    public String getFf4jAuditViewName() {
        return ff4jAuditViewName;
    }

    /**
     * Setter accessor for attribute 'ff4jAuditViewName'.
     * @param ff4jAuditViewName
     * 		new value for 'ff4jAuditViewName '
     */
    public void setFf4jAuditViewName(String ff4jAuditViewName) {
        this.ff4jAuditViewName = ff4jAuditViewName;
    }

    /**
     * Getter accessor for attribute 'ff4jAuditBucketPassword'.
     *
     * @return
     *       current value of 'ff4jAuditBucketPassword'
     */
    public String getFf4jAuditBucketPassword() {
        return ff4jAuditBucketPassword;
    }

    /**
     * Setter accessor for attribute 'ff4jAuditBucketPassword'.
     * @param ff4jAuditBucketPassword
     * 		new value for 'ff4jAuditBucketPassword '
     */
    public void setFf4jAuditBucketPassword(String ff4jAuditBucketPassword) {
        this.ff4jAuditBucketPassword = ff4jAuditBucketPassword;
    }

    /**
     * Setter accessor for attribute 'userName'.
     * @param userName
     * 		new value for 'userName '
     */
    public void setUserName(String userName) {
        this.userName = userName;
    }

    /**
     * Setter accessor for attribute 'password'.
     * @param password
     * 		new value for 'password '
     */
    public void setPassword(String password) {
        this.password = password;
    }
    
    /**
     * Setter accessor for attribute 'userName'.
     * @param userName
     *      new value for 'userName '
     */
    public CouchbaseConnection userName(String userName) {
        setUserName(userName);
        return this;
    }

    /**
     * Setter accessor for attribute 'password'.
     * @param password
     *      new value for 'password '
     */
    public CouchbaseConnection password(String password) {
        setPassword(password);
        return this;
    }

    /**
     * Setter accessor for attribute 'connectionString'.
     * @param connectionString
     * 		new value for 'connectionString '
     */
    public void setConnectionString(String connectionString) {
        this.connectionString = connectionString;
    }
    
    /**
     * Setter accessor for attribute 'connectionString'.
     * @param connectionString
     *      new value for 'connectionString '
     */
    public CouchbaseConnection connectionString(String connectionString) {
        setConnectionString(connectionString);
        return this;
    }
}
