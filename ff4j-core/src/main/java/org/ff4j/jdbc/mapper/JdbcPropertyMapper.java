package org.ff4j.jdbc.mapper;

import java.sql.Connection;
import java.sql.PreparedStatement;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Map;

import org.ff4j.feature.exception.FeatureAccessException;
import org.ff4j.feature.strategy.TogglePredicate;
import org.ff4j.jdbc.JdbcConstants.PropertyColumns;
import org.ff4j.jdbc.JdbcQueryBuilder;
import org.ff4j.mapper.PropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyEvaluationStrategy;
import org.ff4j.property.domain.PropertyFactory;
import org.ff4j.property.domain.PropertyString;
import org.ff4j.property.exception.PropertyAccessException;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.Util;

/**
 * Convert resultset into {@link PropertyString}.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcPropertyMapper extends AbstractJdbcMapper  implements PropertyMapper < PreparedStatement, ResultSet > {
    
    /**
     * Constructor with parameters.
     *
     * @param sqlConn
     *      connection sql
     * @param qbd
     *      query builder
     */
    public JdbcPropertyMapper(Connection sqlConn, JdbcQueryBuilder qbd) {
        super(sqlConn, qbd);
    }
    
    /** {@inheritDoc} */
    public PreparedStatement customPropertytoStore(Property<?> property, String featureId) {
        PreparedStatement ps;
        try {
            ps = sqlConn.prepareStatement(queryBuilder.sqlInsertCustomProperties());
            populatePrepareStatement(property, ps);
            ps.setString(12, featureId);
        } catch (SQLException sqlEx) {
            throw new FeatureAccessException("Cannot create statement to create feature", sqlEx);
        }
        return ps;
    }
    
    /**
     * Propvision user to generate query.
     * @param property
     * @param ps
     * @throws SQLException
     */
    private void populatePrepareStatement(Property<?> property, PreparedStatement ps)
    throws SQLException {
        populateEntity(ps, property);
        // Clazz
        ps.setString(7, property.getType());
        // Value
        ps.setString(8, property.asString());
        // Evaluation Strategy + InitParams
        String strategy  = null;
        String initParam = null;
        if (property.getEvaluationStrategy().isPresent()) {
            PropertyEvaluationStrategy<?> pes = property.getEvaluationStrategy().get();
            strategy  = pes.getClass().getCanonicalName();
            initParam = JsonUtils.mapAsJson(pes.getInitParams());
        }
        ps.setString(9, strategy);
        ps.setString(10, initParam);
        if (property.getFixedValues().isPresent()) {
            String fixedValues = property.getFixedValues().get().toString();
            ps.setString(11, fixedValues.substring(1, fixedValues.length() - 1));
        } else {
            ps.setString(11, null);
        }
    }
    /** {@inheritDoc} */
    @Override
    public PreparedStatement mapToRepository(Property<?> property) {
        PreparedStatement ps;
        try {
            ps = sqlConn.prepareStatement(queryBuilder.sqlInsertProperty());
            populatePrepareStatement(property, ps);
        } catch (SQLException sqlEx) {
            throw new FeatureAccessException("Cannot create statement to create feature", sqlEx);
        }
        return ps;
    }

    /** {@inheritDoc} */
    @Override
    @SuppressWarnings({"rawtypes","unchecked"})
    public Property<?> mapFromRepository(ResultSet rs) {
        try {
            Property<?> p = PropertyFactory.createProperty(
                    rs.getString(PropertyColumns.UID.colname()),  
                    rs.getString(PropertyColumns.CLASSNAME.colname()), 
                    rs.getString(PropertyColumns.VALUE.colname()));
            
            // Common properties for entities (description, owner...)
            mapEntity(rs, p);
            
            // Special elements
            p.setReadOnly(rs.getInt(PropertyColumns.READONLY.colname()) == 1);
            String fixedValues  = rs.getString(PropertyColumns.FIXEDVALUES.colname());
            if (Util.hasLength(fixedValues)) {
                Arrays.stream(fixedValues.split(",")).forEach(v-> p.add2FixedValueFromString(v.trim()) );
            }
            
            // Eval Strategy
            String strategy = rs.getString(PropertyColumns.STRATCLASS.colname());
            if (Util.hasLength(strategy)) {
                Map < String, String > initParams = JsonUtils.jsonAsMap(rs.getString(PropertyColumns.INITPARAMS.colname()));
                TogglePredicate ff4jStrategy = TogglePredicate.of(p.getUid(), strategy, initParams);
                if (ff4jStrategy instanceof PropertyEvaluationStrategy) {
                    p.setEvaluationStrategy((PropertyEvaluationStrategy) ff4jStrategy);
                } else {
                    throw new IllegalArgumentException("Property '" + p.getUid() 
                            + "' has invalid strategy in (" + PropertyColumns.CLASSNAME.colname()
                            + ") with " + ff4jStrategy 
                            + "' as does not extend " + PropertyEvaluationStrategy.class.getName());
                }
            }
            
            return p;
        } catch (SQLException sqlEx) {
            throw new PropertyAccessException("Cannot map Resultset into property", sqlEx);
        }
    }
    
}
