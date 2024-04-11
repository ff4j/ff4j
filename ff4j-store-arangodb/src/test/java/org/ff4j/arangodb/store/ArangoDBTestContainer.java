package org.ff4j.arangodb.store;

/*-
 * #%L
 * ff4j-store-arangodb
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

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.Wait;

/**
 * Test container's implementation for ArangoDB
 */
public class ArangoDBTestContainer extends GenericContainer<ArangoDBTestContainer> {

    private static final int ORIGINAL_PORT = 8529;

    public ArangoDBTestContainer() {
        super("arangodb/arangodb:latest");
        withExposedPorts(ORIGINAL_PORT);
        withEnv("ARANGO_NO_AUTH", "1");
        waitingFor(Wait.forLogMessage(".*is ready for business.*", 1));
    }
}
