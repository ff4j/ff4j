package org.ff4j.springjdbc.store.rowmapper;

/*
 * #%L
 * ff4j-store-springjdbc
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

import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.JdbcPropertyMapper;
import org.springframework.jdbc.core.RowMapper;

/**
 * Convert resultset into {@link PropertyString}.
 */
public class CustomPropertyRowMapper extends JdbcPropertyMapper implements RowMapper<Property<?>> {

    /** {@inheritDoc} */
    public Property<?> mapRow(ResultSet rs, int row) throws SQLException {
        return super.map(rs);
    }

}
