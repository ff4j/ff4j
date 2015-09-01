package org.ff4j.property.store;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.ff4j.property.AbstractProperty;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyFactory;
import org.ff4j.store.JdbcStoreConstants;

/**
 * Convert resultset into {@link Property}.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcPropertyMapper implements JdbcStoreConstants {
    
    /**
     * Expect to convert a JDBC Result to Property.
     * @param rs
     *      current line resultset
     * @return
     *      target property
     * @throws SQLException
     */
    public AbstractProperty<?> map(ResultSet rs) throws SQLException {
        String propertyName  = rs.getString(COL_PROPERTY_ID);
        String propertyValue = rs.getString(COL_PROPERTY_VALUE);
        String propertyType  = rs.getString(COL_PROPERTY_TYPE);
        String description   = rs.getString(COL_PROPERTY_DESCRIPTION);
        String fixedValues   = rs.getString(COL_PROPERTY_FIXED);
        return PropertyFactory.createProperty(propertyName, propertyType, propertyValue, description, fixedValues);
    }
    
}
