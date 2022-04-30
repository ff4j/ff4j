package org.ff4j.gcpdatastore.store;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.Wait;

/*
 * #%L
 * ff4j-store-arangodb
 * %%
 * Copyright (C) 2022 FF4J
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

public class GcpDatastoreTestContainer extends GenericContainer<GcpDatastoreTestContainer> {
    private static final int PORT = 8081;
    private static final String[] COMMAND = {"gcloud", "beta", "emulators", "datastore", "start",
            "--host-port=0.0.0.0:" + PORT, "--quiet"};

    public GcpDatastoreTestContainer() {
        super("gcr.io/google.com/cloudsdktool/cloud-sdk:emulators");
        withExposedPorts(PORT);
        withCommand(COMMAND);
        withEnv("CLOUDSDK_CORE_PROJECT", "ff4j-store-gcp-datastore-project");
        waitingFor(Wait.forLogMessage(".*Dev App Server is now running.*", 1));
    }
}
