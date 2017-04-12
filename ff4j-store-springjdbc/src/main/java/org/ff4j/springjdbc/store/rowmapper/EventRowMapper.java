package org.ff4j.springjdbc.store.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.ff4j.audit.Event;
import org.ff4j.store.JdbcEventMapper;
import org.springframework.jdbc.core.RowMapper;

/**
 * Mapper to unmarshell an {@link Event} from result.
 * 
 * Leveraging on existing {@link JdbcEventMapper}
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class EventRowMapper extends JdbcEventMapper implements RowMapper<Event> {

    /** {@inheritDoc} */
    @Override
    public Event mapRow(ResultSet rs, int rowNum) throws SQLException {
        return mapEvent(rs);
    } 

}
