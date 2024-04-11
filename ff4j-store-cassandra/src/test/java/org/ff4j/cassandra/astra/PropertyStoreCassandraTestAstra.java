package org.ff4j.cassandra.astra;

/*-
 * #%L
 * ff4j-store-cassandra
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

import java.io.File;

import org.ff4j.cassandra.AsbtractPropertyStoreCassandraTest;
import org.junit.Ignore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.config.DriverConfigLoader;

@Ignore
public class PropertyStoreCassandraTestAstra extends AsbtractPropertyStoreCassandraTest {

    /** Logger for the class. */
    private static Logger LOGGER = LoggerFactory.getLogger(PropertyStoreCassandraTestAstra.class);

    /** {@inheritDoc} */
    @Override
    public CqlSession initCqlSession() {
        String configFile = 
                PropertyStoreCassandraTestAstra.class.getResource("/application_astra.conf").getFile();
        DriverConfigLoader configLoader = 
                DriverConfigLoader.fromFile(new File(configFile));
        CqlSession cqlSession = 
                CqlSession.builder().withConfigLoader(configLoader).build();
        LOGGER.info("Connection Established to Astra with Keyspace {}", cqlSession.getKeyspace().get());
        return cqlSession;
    }

}
