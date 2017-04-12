package org.ff4j.springjdbc.store.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.ff4j.audit.MutableHitCount;
import org.ff4j.springjdbc.store.dto.HitCountDto;
import org.springframework.jdbc.core.RowMapper;

/**
 * Technical Row Mapper.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class HitCountRowMapper implements RowMapper<HitCountDto>{
    
    /** 
     * Column to extract from db.
     */
    private String columnName;
    
    public HitCountRowMapper(String col) {
        this.columnName = col;
    }

    /** {@inheritDoc} */
    @Override
    public HitCountDto mapRow(ResultSet rs, int rowNum) throws SQLException {
        return new HitCountDto(rs.getString(columnName), new MutableHitCount(rs.getInt("NB")));
    }

}
