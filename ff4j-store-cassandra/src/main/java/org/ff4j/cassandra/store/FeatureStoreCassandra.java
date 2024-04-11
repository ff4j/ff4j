package org.ff4j.cassandra.store;

/*-
 * #%L
 * ff4j-store-cassandra
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

import static com.datastax.oss.driver.api.querybuilder.QueryBuilder.literal;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.ff4j.cassandra.FF4jCassandraSchema;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.MappingUtil;
import org.ff4j.utils.Util;

import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.cql.BatchStatement;
import com.datastax.oss.driver.api.core.cql.BatchStatementBuilder;
import com.datastax.oss.driver.api.core.cql.BatchType;
import com.datastax.oss.driver.api.core.cql.BoundStatement;
import com.datastax.oss.driver.api.core.cql.PreparedStatement;
import com.datastax.oss.driver.api.core.cql.ResultSet;
import com.datastax.oss.driver.api.core.cql.Row;
import com.datastax.oss.driver.api.core.data.UdtValue;
import com.datastax.oss.driver.api.core.type.UserDefinedType;
import com.datastax.oss.driver.api.querybuilder.QueryBuilder;
import com.datastax.oss.driver.shaded.guava.common.base.Functions;

/**
 * Implementation of {@link FeatureStore} to work with Cassandra Storage.
 * 
 * Minimize the Number of Writes : 
 * Writes in Cassandra aren?t free, but they?re awfully cheap. Cassandra is optimized for high write throughput, 
 * and almost all writes are equally efficient [1]. If you can perform extra writes to improve the efficiency of
 * your read queries, it?s almost always a good tradeoff. Reads tend to be more expensive and are much more 
 * difficult to tune.
 * 
 * Minimize Data Duplication
 * Denormalization and duplication of data is a fact of life with Cassandra. Don?t be afraid of it. Disk space 
 * is generally the cheapest resource (compared to CPU, memory, disk IOPs, or network), and Cassandra is 
 * architected around that fact. In order to get the most efficient reads, you often need to duplicate data.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class FeatureStoreCassandra extends AbstractFeatureStore implements FF4jCassandraSchema {
    
    /** Driver Session. */
    private CqlSession cqlSession;
    
    /** udt. */
    private UserDefinedType udtStrategy;
    private UserDefinedType udtProperty;
    
    /** Statements. */
    private PreparedStatement psExistFeature;
    private PreparedStatement psToggleFeature;
    private PreparedStatement psInsertFeature;
    private PreparedStatement psDeleteFeature;
    private PreparedStatement psReadFeature;
    private PreparedStatement psReadGroup;
    private PreparedStatement psAddToGroup;
    private PreparedStatement psRmvFromGroup;
    private PreparedStatement psListGroups;
    
    /**
     * Default constructor.
     */
    public FeatureStoreCassandra() {}
    
    /**
     * Connector with running session
     */
    public FeatureStoreCassandra(CqlSession cqlSession) {
        this.cqlSession = cqlSession;
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        cqlSession.execute(STMT_CREATE_UDT_STRATEGY);
        cqlSession.execute(STMT_CREATE_UDT_PROPERTY);
        cqlSession.execute(STMT_CREATE_TABLE_FEATURE);
        cqlSession.execute(STMT_CREATE_INDEX_FEATUREGROUP);
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertHasLength(uid);
        return getCqlSession().execute(psExistFeature.bind(uid))
                              .getAvailableWithoutFetching() > 0;
    }
    
    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        assertFeatureExist(uid);
        getCqlSession().execute(psToggleFeature.bind(true, uid));
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        assertFeatureExist(uid);
        getCqlSession().execute(psToggleFeature.bind(false, uid));
    }    

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureNotExist(fp.getUid());
        
        BoundStatement bsInsertFeature = psInsertFeature.bind();
        bsInsertFeature = bsInsertFeature.setString(FEATURES_ATT_UID, fp.getUid());
        bsInsertFeature = bsInsertFeature.setBoolean(FEATURES_ATT_ENABLED, fp.isEnable());
        bsInsertFeature = bsInsertFeature.setString(FEATURES_ATT_DESCRIPTION, fp.getDescription());
        if (Util.hasLength(fp.getGroup())) {
            bsInsertFeature = bsInsertFeature.setString(FEATURES_ATT_GROUPNAME, fp.getGroup());
        }
        if (null != fp.getPermissions()) {
            bsInsertFeature = bsInsertFeature.setSet(FEATURES_ATT_ROLES, fp.getPermissions(), String.class);
        }
        if (null != fp.getFlippingStrategy()) {
            UdtValue newUdtStrategy = udtStrategy.newValue();
            FlippingStrategy fStrategy = fp.getFlippingStrategy();
            newUdtStrategy.setString(UDT_STRATEGY_CLASS, fStrategy.getClass().getName());
            newUdtStrategy.setMap(UDT_STRATEGY_PARAMS, fStrategy.getInitParams(), String.class, String.class);
            bsInsertFeature = bsInsertFeature.setUdtValue(FEATURES_ATT_STRATEGY, newUdtStrategy);
        }
        if (null != fp.getCustomProperties()) {
            Map<String, UdtValue> properties = new HashMap<>();
            for (Property<?> prop : fp.getCustomProperties().values()) {
                UdtValue currentUdtProp = udtProperty.newValue();
                currentUdtProp.setString(UDT_PROPERTY_UID, prop.getName());
                currentUdtProp.setString(UDT_PROPERTY_CLASS, prop.getClass().getName());
                currentUdtProp.setString(UDT_PROPERTY_DESCRIPTION, prop.getDescription());
                currentUdtProp.setString(UDT_PROPERTY_VALUE, prop.asString());
                Set <String> fixedValues = new HashSet<>();
                if (!Util.isEmpty(prop.getFixedValues())) {
                    for (Object fv : prop.getFixedValues()) {
                        fixedValues.add(fv.toString());
                    }
                }
                currentUdtProp.setSet(UDT_PROPERTY_FIXEDVALUES, fixedValues, String.class);
                properties.put(prop.getName(), currentUdtProp);
            }
            bsInsertFeature = bsInsertFeature.setMap(FEATURES_ATT_PROPERTIES, properties, String.class, UdtValue.class);
        }
        getCqlSession().execute(bsInsertFeature);
    }
    
    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        assertFeatureExist(uid);
        getCqlSession().execute(psDeleteFeature.bind(uid));
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        Util.assertHasLength(uid);
        ResultSet rs = cqlSession.execute(psReadFeature.bind(uid));
        Row row = rs.one();
        if (null == row) {
            throw new FeatureNotFoundException(uid);
        }
        return mapFeatureRow(row);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Map < String, Feature> features = new HashMap<String, Feature>();
        ResultSet rs = cqlSession.execute(STMT_FEATURE_READ_ALL);
        for (Row row : rs.all()) {
            features.put(row.getString(FEATURES_ATT_UID), mapFeatureRow(row));
        }
        return features;
    }
    
    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureExist(fp.getUid());
        delete(fp.getUid());
        create(fp);
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        assertFeatureExist(uid);
        Util.assertHasLength(roleName);
        getCqlSession().execute(QueryBuilder.update(FEATURES_TABLE)
                .appendSetElement(FEATURES_ATT_ROLES, literal(roleName))
                .whereColumn(FEATURES_ATT_UID).isEqualTo(literal(uid))
                .build());
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        assertFeatureExist(uid);
        Util.assertHasLength(roleName);
        getCqlSession().execute(QueryBuilder.update(FEATURES_TABLE)
                .removeSetElement(FEATURES_ATT_ROLES, literal(roleName))
                .whereColumn(FEATURES_ATT_UID).isEqualTo(literal(uid))
                .build());
    }
    
    /*
     * Unfortunately no way to apply update on multiple features in 1 call.
     * We go N+1 select, we can still group in a batch statement.
     */
    @Override
    public void enableGroup(String groupName) {
        assertGroupExist(groupName);
        BatchStatementBuilder stmtBatch = BatchStatement.builder(BatchType.LOGGED);
        readGroup(groupName).values()
                            .stream().map(Feature::getUid)
                            .forEach(uid -> stmtBatch.addStatement(psToggleFeature.bind(true, uid)));
        getCqlSession().execute(stmtBatch.build());
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        assertGroupExist(groupName);
        BatchStatementBuilder stmtBatch = BatchStatement.builder(BatchType.LOGGED);
        readGroup(groupName).values().stream()
                            .map(Feature::getUid)
                            .forEach(uid -> stmtBatch.addStatement(psToggleFeature.bind(false, uid)));
        getCqlSession().execute(stmtBatch.build());
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertHasLength(groupName);
        return getCqlSession().execute(psReadGroup.bind(groupName))
                         .getAvailableWithoutFetching() > 0;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        assertGroupExist(groupName);
        return getCqlSession().execute(psReadGroup.bind(groupName))
                         .all().stream().map(this::mapFeatureRow)
                         .collect(Collectors.toMap(Feature::getUid, Functions.identity()));
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        Util.assertHasLength(groupName);
        getCqlSession().execute(psAddToGroup.bind(groupName, uid));
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        assertGroupExist(groupName);
        Feature feat = read(uid);
        if (feat.getGroup() != null && !feat.getGroup().equals(groupName)) {
            throw new IllegalArgumentException("'" + uid + "' is not in group '" + groupName + "'");
        }
        getCqlSession().execute(psRmvFromGroup.bind(uid));
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        List < Row> rows = cqlSession.execute(psListGroups.bind()).all();
        Set<String> groupNames = new HashSet<>();
        if (null != rows) {
            rows.stream()
                .map(r -> r.getString(FEATURES_ATT_GROUPNAME))
                .forEach(groupNames::add);
        }
        groupNames.remove(null);
        groupNames.remove("");
        return groupNames;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        getCqlSession().execute(QueryBuilder.truncate(FEATURES_TABLE).build());
    }
    
    /**
     * Prepared once, run many.
     */
    protected void prepareStatements() {
        
        // udt
        udtStrategy = cqlSession.getMetadata()
                .getKeyspace(cqlSession.getKeyspace().get())
                .flatMap(ks -> ks.getUserDefinedType(UDT_STRATEGY))
                .orElseThrow(() -> new IllegalArgumentException("Missing UDT '" + UDT_STRATEGY + "'"));
        udtProperty = cqlSession.getMetadata()
                .getKeyspace(cqlSession.getKeyspace().get())
                .flatMap(ks -> ks.getUserDefinedType(UDT_PROPERTY))
                .orElseThrow(() -> new IllegalArgumentException("Missing UDT '" + UDT_PROPERTY + "'"));
        
        // Prepared Statements
        psReadFeature   = cqlSession.prepare(STMT_FEATURE_READ);
        psExistFeature  = cqlSession.prepare(STMT_FEATURE_EXIST);
        psToggleFeature = cqlSession.prepare(STMT_FEATURE_TOGGLE);
        psInsertFeature = cqlSession.prepare(STMT_FEATURE_INSERT);
        psDeleteFeature = cqlSession.prepare(STMT_FEATURE_DELETE);
        psReadGroup     = cqlSession.prepare(STMT_FEATUREGROUP_READ);
        psAddToGroup    = cqlSession.prepare(STMT_FEATURE_ADDTOGROUP);
        psRmvFromGroup  = cqlSession.prepare(STMT_FEATURE_REMOVEGROUP);
        psListGroups    = cqlSession.prepare(STMT_FEATUREGROUP_LIST);
    }
    
    protected Feature mapFeatureRow(Row row) {
        Feature f = new Feature(row.getString(FEATURES_ATT_UID));
        f.setDescription(row.getString(FEATURES_ATT_DESCRIPTION));
        f.setEnable(row.getBoolean(FEATURES_ATT_ENABLED));
        f.setGroup(row.getString(FEATURES_ATT_GROUPNAME));
        f.setPermissions(row.getSet(FEATURES_ATT_ROLES, String.class));
        // Flipping Strategy
        UdtValue udtStrat = row.getUdtValue(FEATURES_ATT_STRATEGY);
        if (null != udtStrat) {
            String className = udtStrat.getString(UDT_STRATEGY_CLASS);
            Map <String, String > initparams = udtStrat.getMap(UDT_STRATEGY_PARAMS,String.class, String.class);
            f.setFlippingStrategy(MappingUtil.instanceFlippingStrategy(f.getUid(), className, initparams));
        }
        // Custom Properties
        Map <String, UdtValue> mapOfProperties = row.getMap(FEATURES_ATT_PROPERTIES, String.class, UdtValue.class);
        if (mapOfProperties != null) {
            Map < String, Property<?>> customProperties = new HashMap<String, Property<?>>();
            for(UdtValue udt : mapOfProperties.values()) {
                String propName  = udt.getString(UDT_PROPERTY_UID);
                String propClass = udt.getString(UDT_PROPERTY_CLASS);
                String propDesc  = udt.getString(UDT_PROPERTY_DESCRIPTION);
                String propVal   = udt.getString(UDT_PROPERTY_VALUE);
                Set<String> fixV = udt.getSet(UDT_PROPERTY_FIXEDVALUES, String.class);
                Property<?> p = PropertyFactory.createProperty(propName, propClass, propVal, propDesc, fixV);
                customProperties.put(p.getName(), p);
            }
            f.setCustomProperties(customProperties);
        }
        return f;
    }
    
    /**
     * Prepared statements on first call.
     */
    private synchronized CqlSession getCqlSession() {
        if (null == psExistFeature) {
            prepareStatements();
        }
        return cqlSession;
    }
}
