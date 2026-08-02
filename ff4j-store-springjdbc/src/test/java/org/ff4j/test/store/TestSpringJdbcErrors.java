package org.ff4j.test.store;

/*-
 * #%L
 * ff4j-store-springjdbc
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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


import static org.junit.jupiter.api.Assertions.assertThrows;


import org.ff4j.springjdbc.store.FeatureStoreSpringJdbc;
import org.ff4j.springjdbc.store.PropertyStoreSpringJdbc;
import org.junit.jupiter.api.Test;

public class TestSpringJdbcErrors {
    
    @Test
    public void testErrorOnDataSource() {
        assertThrows(IllegalStateException.class, () -> {
            FeatureStoreSpringJdbc s = new FeatureStoreSpringJdbc();
            s.getJdbcTemplate();
        });
    }
    
    @Test
    public void testErrorOnDataSourceProperty() {
        assertThrows(IllegalStateException.class, () -> {
            PropertyStoreSpringJdbc s = new PropertyStoreSpringJdbc();
            s.getJdbcTemplate();
        });
    }

}
