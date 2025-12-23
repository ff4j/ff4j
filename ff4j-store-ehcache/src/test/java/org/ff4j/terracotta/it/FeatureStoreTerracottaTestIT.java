package org.ff4j.terracotta.it;

/*-
 * #%L
 * ff4j-store-ehcache
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

import com.github.dockerjava.api.model.ExposedPort;
import com.github.dockerjava.api.model.PortBinding;
import net.sf.ehcache.config.CacheConfiguration;
import net.sf.ehcache.config.Configuration;
import net.sf.ehcache.config.MemoryUnit;
import net.sf.ehcache.config.TerracottaClientConfiguration;
import net.sf.ehcache.config.TerracottaConfiguration;
import net.sf.ehcache.terracotta.TerracottaCacheCluster;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.ehcache.FF4jEhCacheWrapper;
import org.ff4j.store.FeatureStoreEhCache;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Disabled;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.util.Map;

/**
 * Test to work with Redis as a store.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 *
 * Working but quite slow to test all on each build
 */
@Disabled
@Testcontainers
public class FeatureStoreTerracottaTestIT extends FeatureStoreTestSupport {

    private static final Logger log = LoggerFactory.getLogger(FeatureStoreTerracottaTestIT.class);

    private static final int TERRACOTTA_PORT = 9510;
    private static final int TERRACOTTA_SYNC_PORT = 9530;

    @Container
    private static final GenericContainer<?> terracottaContainer =
            new GenericContainer<>("terracotta/terracotta-server-oss:4.3.5")
                    .withCreateContainerCmdModifier(it -> it
                            .withHostName("localhost")
                            .getHostConfig()
                            .withPortBindings(
                                    new PortBinding(
                                            com.github.dockerjava.api.model.Ports.Binding.bindPort(TERRACOTTA_PORT),
                                            new ExposedPort(TERRACOTTA_PORT)
                                    ),
                                    new PortBinding(
                                            com.github.dockerjava.api.model.Ports.Binding.bindPort(TERRACOTTA_SYNC_PORT),
                                            new ExposedPort(TERRACOTTA_SYNC_PORT)
                                    )
                            )
                            .withAutoRemove(true)
                    )
                    // This is important so that Ryuk kills this container even if you have this feature enabled
                    .withReuse(false);


    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        // Configuration to work with Terracotta
        String terracottaUrl = String.format("localhost:%d", TERRACOTTA_PORT);
        log.info("Terracotta server URL={} ", terracottaUrl);

        Configuration managerConfiguration = new Configuration();
        managerConfiguration.name("config")
            .terracotta(new TerracottaClientConfiguration()
                    .url(terracottaUrl)
                    .wanEnabledTSA(false)
            )

            .defaultCache(new CacheConfiguration()
                .maxBytesLocalHeap(128, MemoryUnit.MEGABYTES)
                .terracotta(new TerracottaConfiguration()))

            .cache(new CacheConfiguration()
                .name(FF4jEhCacheWrapper.CACHENAME_FEATURES)
                .maxBytesLocalHeap(128, MemoryUnit.MEGABYTES)
                .terracotta(new TerracottaConfiguration())
            );

        FeatureStoreEhCache ehcacheStore = new FeatureStoreEhCache(managerConfiguration);
        ehcacheStore.importFeaturesFromXmlFile("ff4j.xml");

        return ehcacheStore;
    }

    /**
     * Clean store after each test (avoid duplication)
     */
    @AfterEach
    public void cleanStore() {
        Map<String, Feature> f = testedStore.readAll();
        for (String key : f.keySet()) {
            testedStore.delete(key);
        }
    }

}
