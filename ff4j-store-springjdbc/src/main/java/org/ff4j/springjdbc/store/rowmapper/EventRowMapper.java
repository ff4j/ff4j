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
