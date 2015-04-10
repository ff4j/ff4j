package org.ff4j.store;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.utils.ParameterUtils;

/**
 * Map resultset into {@link Feature}
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcFeatureMapper implements JdbcStoreConstants {
    
    /**
     * Map feature result to bean.
     * 
     * @param rs
     *            current resultSet
     * @return current Feature without roles
     * @throws SQLException
     *             error accured when parsing resultSet
     */
    public Feature mapFeature(ResultSet rs) throws SQLException {
        // Feature
        Feature f = null;
        boolean enabled = rs.getInt(COL_FEAT_ENABLE) > 0;
        String featUid = rs.getString(COL_FEAT_UID);
        f = new Feature(featUid, enabled, rs.getString(COL_FEAT_DESCRIPTION), rs.getString(COL_FEAT_GROUPNAME));
        // Strategy
        String strategy = rs.getString(COL_FEAT_STRATEGY);
        if (strategy != null && !"".equals(strategy)) {
            try {
                FlippingStrategy flipStrategy = (FlippingStrategy) Class.forName(strategy).newInstance();
                flipStrategy.init(featUid, ParameterUtils.toMap(rs.getString(COL_FEAT_EXPRESSION)));
                f.setFlippingStrategy(flipStrategy);
            } catch (InstantiationException ie) {
                throw new FeatureAccessException("Cannot instantiate Strategy, no default constructor available", ie);
            } catch (IllegalAccessException iae) {
                throw new FeatureAccessException("Cannot instantiate Strategy, no visible constructor", iae);
            } catch (ClassNotFoundException e) {
                throw new FeatureAccessException("Cannot instantiate Strategy, classNotFound", e);
            }
        }
        return f;
    }


}
