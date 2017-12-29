package org.ff4j.v1.store;

import static org.ff4j.v1.store.JdbcStoreConstants.*;
import static org.ff4j.v1.utils.MappingUtil.instanceFlippingStrategy;
import static org.ff4j.v1.utils.MappingUtil.toMap;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;

import org.ff4j.v1.core.Feature;

/**
 * Map resultset into {@link Feature}
 *
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcFeatureMapper {
    
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
        Feature f;
        boolean enabled = rs.getInt(COL_FEAT_ENABLE) > 0;
        String featUid = rs.getString(COL_FEAT_UID);
        f = new Feature(featUid, enabled, rs.getString(COL_FEAT_DESCRIPTION), rs.getString(COL_FEAT_GROUPNAME));
        // Strategy
        String strategy = rs.getString(COL_FEAT_STRATEGY);
        if (strategy != null && !"".equals(strategy)) {
            Map < String, String > initParams = toMap(rs.getString(COL_FEAT_EXPRESSION));
            f.setFlippingStrategy(instanceFlippingStrategy(featUid, strategy, initParams));
        }
        return f;
    }

}
