package org.ff4j.springjdbc.store.rowmapper;

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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.springframework.jdbc.core.RowMapper;

import static org.ff4j.store.JdbcStoreConstants.*;

/**
 * Implementation of JDBC store using Spring JDBC.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class RoleRowMapper implements RowMapper<Integer> {

    /** Default Row Mapper to get groups. */
    private final Map<String, Set<String>> roles = new HashMap<String, Set<String>>();

    /** {@inheritDoc} */
    @Override
    public Integer mapRow(ResultSet rs, int rowNum) throws SQLException {
        String featId = rs.getString(COL_ROLE_FEATID);
        if (!roles.containsKey(featId)) {
            roles.put(featId, new HashSet<String>());
        }
        roles.get(featId).add(rs.getString(COL_ROLE_ROLENAME));
        return rowNum;
    }

    /**
     * Getter accessor for attribute 'roles'.
     *
     * @return current value of 'roles'
     */
    public Map<String, Set<String>> getRoles() {
        return roles;
    }

}
