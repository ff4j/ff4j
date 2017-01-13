package org.ff4j.elastic.store;

import java.net.MalformedURLException;
import java.net.URL;

import org.elasticsearch.node.Node;

/*
 * #%L
 * ff4j-store-elastic
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

import org.ff4j.core.FeatureStore;
import org.ff4j.elastic.ElasticConnection;
import org.ff4j.elastic.ElasticConnectionMode;
import org.ff4j.elastic.server.EmbeddedElasticServer;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Ignore;
import org.junit.rules.TemporaryFolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:andre.blaszczyk@gmail.com">Andre Blaszczyk</a>
 *
 */
@Ignore
public class FeatureStoreElasticTest extends FeatureStoreTestSupport {

	private final static Logger logger = LoggerFactory.getLogger(FeatureStoreElasticTest.class);

	/**
	 * Using temporary folder as path data for Elasticsearch.
	 */
	@ClassRule
	public final static TemporaryFolder folder = new TemporaryFolder();

	private static Node server;

	@BeforeClass
	public static void setup() {
		server = EmbeddedElasticServer.builder() //
				.clusterName("myIntegrationClusterFeature") //
				.dataDirectory(folder.getRoot().getPath()) //
				.health(true) //
				.start();
	}

	@AfterClass
	public static void tearDown() {
		server.close();
	}

	@Override
	protected int enablePause() {
		// waiting 2 seconds for integration tests.
		return 2;
	}

	@Override
	protected FeatureStore initStore() {
		ElasticConnection connection = null;
		try {
			connection = new ElasticConnection(ElasticConnectionMode.JEST_CLIENT, "ff4j",
					new URL("http://localhost:9200"));
		} catch (MalformedURLException e) {
			logger.error(e.getMessage(), e);
		}
		return new FeatureStoreElastic(connection, "test-ff4j-features.xml");
	}
}
