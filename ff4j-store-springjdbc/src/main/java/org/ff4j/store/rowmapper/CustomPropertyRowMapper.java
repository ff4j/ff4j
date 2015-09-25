package org.ff4j.store.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.ff4j.property.AbstractProperty;
import org.ff4j.property.Property;
import org.ff4j.property.store.JdbcPropertyMapper;
import org.springframework.jdbc.core.RowMapper;

/**
 * Convert resultset into {@link Property}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class CustomPropertyRowMapper extends JdbcPropertyMapper implements RowMapper<AbstractProperty<?>> {

    @Override
    public AbstractProperty<?> mapRow(ResultSet rs, int row)
            throws SQLException {
        return super.map(rs);
    }

}
