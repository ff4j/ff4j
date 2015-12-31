package org.ff4j.store.rowmapper;

/*
 * #%L
 * ff4j-store-jdbc
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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
import org.ff4j.store.JdbcStoreConstants;
import org.ff4j.utils.MappingUtil;
import org.springframework.jdbc.core.RowMapper;

/**
 * Mapper to convert result into
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureRowMapper implements RowMapper<Feature>, JdbcStoreConstants {

    /** {@inheritDoc} */
    @Override
    public Feature mapRow(ResultSet rs, int rowNum) throws SQLException {
        String featUid = rs.getString(COL_FEAT_UID);
        Feature f = new Feature(featUid, rs.getInt(COL_FEAT_ENABLE) > 0);
        f.setDescription(rs.getString(COL_FEAT_DESCRIPTION));
        f.setGroup(rs.getString(COL_FEAT_GROUPNAME));

        // Build Flipping Strategy From DataBase
        String strategy = rs.getString(COL_FEAT_STRATEGY);
        if (strategy != null && !"".equals(strategy)) {
            try {
                FlippingStrategy flipStrategy = (FlippingStrategy) Class.forName(strategy).newInstance();
                flipStrategy.init(featUid, MappingUtil.toMap(rs.getString(COL_FEAT_EXPRESSION)));
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
