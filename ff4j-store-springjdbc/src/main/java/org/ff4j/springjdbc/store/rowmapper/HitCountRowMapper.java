package org.ff4j.springjdbc.store.rowmapper;

/*
 * #%L
 * ff4j-store-springjdbc
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
