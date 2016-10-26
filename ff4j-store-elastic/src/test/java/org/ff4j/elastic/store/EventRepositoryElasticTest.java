package org.ff4j.elastic.store;

import java.net.MalformedURLException;
import java.net.URL;

import org.elasticsearch.action.admin.cluster.health.ClusterHealthResponse;
import org.elasticsearch.common.settings.ImmutableSettings;
import org.elasticsearch.node.Node;
import org.elasticsearch.node.NodeBuilder;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.elastic.ElasticConnection;
import org.ff4j.elastic.ElasticConnectionMode;
import org.ff4j.test.audit.EventRepositoryTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.rules.TemporaryFolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EventRepositoryElasticTest extends EventRepositoryTestSupport {

	private final static Logger logger = LoggerFactory.getLogger(EventRepositoryElasticTest.class);

	/**
	 * Using temporary folder as path data for Elasticsearch.
	 */
	@ClassRule
	public final static TemporaryFolder folder = new TemporaryFolder();

	private static Node server;

	@BeforeClass
	public static void setup() {
		server = EmbeddedElasticServer(folder.getRoot().getPath());
		server.start();
		// Display cluster information
		ClusterHealthResponse healths = server.client().admin().cluster().prepareHealth().get();
		logger.info(healths.toString());
	}

	@AfterClass
	public static void tearDown() {
		server.close();
	}

	@Override
	protected EventRepository initRepository() {
		ElasticConnection connection = null;
		try {
			connection = new ElasticConnection(ElasticConnectionMode.JEST_CLIENT, "ff4j",
					new URL("http://localhost:9200"));
		} catch (MalformedURLException e) {
			logger.error(e.getMessage(), e);
		}
		EventRepository elasticStore = new EventRepositoryElastic(connection);
		elasticStore.createSchema();
		return elasticStore;
	}

	// Convenient methods

	public static Node EmbeddedElasticServer(String dataDirectory) {

		ImmutableSettings.Builder elasticSettings = ImmutableSettings.settingsBuilder() //
				.put("path.data", dataDirectory);

		return NodeBuilder.nodeBuilder().clusterName("myIntegrationClusterEvent").local(true)
				.settings(elasticSettings.build()).node();
	}
}
