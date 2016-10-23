package org.ff4j.elastic;

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

import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.elasticsearch.client.Client;
import org.elasticsearch.client.transport.TransportClient;
import org.elasticsearch.common.transport.InetSocketTransportAddress;
import org.elasticsearch.node.NodeBuilder;
import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.property.Property;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import io.searchbox.action.Action;
import io.searchbox.client.JestClient;
import io.searchbox.client.JestClientFactory;
import io.searchbox.client.JestResult;
import io.searchbox.client.config.HttpClientConfig;
import io.searchbox.core.SearchResult;
import io.searchbox.indices.CreateIndex;
import io.searchbox.indices.DeleteIndex;
import io.searchbox.indices.IndicesExists;

/**
 * Poor design by clun, to be challenged !!
 */
public class ElasticConnection {

	/** Logger for the class. */
	private final Logger logger = LoggerFactory.getLogger(ElasticConnection.class);

	private ElasticConnectionMode connectionMode;

	private Client esClient;

	private JestClient jestClient;

	private String indexName;

	private Set<URL> urlSet;

	/**
	 * Proposal to initiate a connection to a elastic cluster.
	 *
	 * @param mode
	 *            choose client to be used
	 * @param url
	 *            target nodes of the cluster (optional)
	 */
	public ElasticConnection(ElasticConnectionMode mode, String indexName, URL... url) {
		Util.assertParamHasNotNull(mode, "ElasticConnectionMode");
		Util.assertParamHasNotNull(indexName, "indexName");
		Util.assertParamHasNotNull(url, "url");
		this.indexName = indexName;
		this.connectionMode = mode;
		this.urlSet = Util.set(url);

		try {
			switch (mode) {
			case NATIVE_CLIENT:
				initNativeClient();
				break;
			case TRANSPORT_CLIENT:
				initTransportClient();
				break;
			case JEST_CLIENT:
				initJestClient();
				break;
			default:
				initJestClient();
			}
		} catch (IOException e) {
			// dedicated runtime exception ElasticConnectionException ?
		}
	}

	private void initTransportClient() {
		TransportClient tClient = new TransportClient();
		if (!Util.isEmpty(urlSet)) {
			for (URL url : urlSet) {
				tClient.addTransportAddress(new InetSocketTransportAddress(url.getHost(), url.getPort()));
			}
		}
		esClient = tClient;
	}

	private void initNativeClient() {
		esClient = NodeBuilder.nodeBuilder().client(true).node().client();
		boolean indexExists = esClient.admin().indices().prepareExists(indexName).execute().actionGet().isExists();
		if (indexExists) {
			esClient.admin().indices().prepareDelete(indexName).execute().actionGet();
		}
		esClient.admin().indices().prepareCreate(indexName).execute().actionGet();
	}

	private void initJestClient() throws IOException {
		JestClientFactory factory = new JestClientFactory();

		// Custom feature converter is required
		Gson gson = new GsonBuilder() //
				.registerTypeAdapter(Feature.class, new FeatureConverter()) //
				.registerTypeAdapter(Property.class, new PropertyConverter()) //
				.create();

		factory.setHttpClientConfig(new HttpClientConfig.Builder(mapUrl()) //
				.multiThreaded(true) //
				.gson(gson) //
				.build());

		jestClient = factory.getObject();
		boolean indexExists = jestClient.execute(new IndicesExists.Builder(indexName).build()).isSucceeded();
		if (indexExists) {
			jestClient.execute(new DeleteIndex.Builder(indexName).build());
		}
		jestClient.execute(new CreateIndex.Builder(indexName).build());
	}

	/**
	 * Common behaviour to execute JEST query.
	 *
	 * @param request
	 *            target request
	 * @return
	 */
	public <T extends JestResult> JestResult execute(Action<T> request, boolean allowFailure) {
		JestResult sr = null;
		try {
			sr = getJestClient().execute(request);
			if (null == sr) {
				throw new FeatureAccessException("Cannot query elastic seach, result was null : check your query");
			}
			if (!allowFailure && !sr.isSucceeded()) {
				throw new FeatureAccessException(
						"Query to Elastic failed - " + sr.getErrorMessage() + " (query=" + request.toString() + ")");
			}
			return sr;
		} catch (IOException e) {
			logger.error(e.getMessage(), e);
			throw new FeatureAccessException(
					"Error with query to Elastic - " + request.toString() + " An exception occured ", e);
		}
	}

	public <T extends JestResult> JestResult execute(Action<T> request) {
		return execute(request, false);
	}

	public <T extends JestResult> SearchResult search(Action<T> request) {
		return search(request, false);
	}

	/**
	 * Syntax suger to search.
	 * 
	 * @param request
	 *            jest request
	 * @return jest result
	 */
	public <T extends JestResult> SearchResult search(Action<T> request, boolean allowFailure) {
		return (SearchResult) execute(request, allowFailure);
	}

	/**
	 * Before Lambda...
	 */
	private Collection<String> mapUrl() {
		Collection<String> collec = new HashSet<String>();
		for (URL url : urlSet) {
			collec.add(url.toString());
		}
		return collec;
	}

	/**
	 * Getter accessor for attribute 'connectionMode'.
	 *
	 * @return current value of 'connectionMode'
	 */
	public ElasticConnectionMode getConnectionMode() {
		return connectionMode;
	}

	/**
	 * Setter accessor for attribute 'connectionMode'.
	 * 
	 * @param connectionMode
	 *            new value for 'connectionMode '
	 */
	public void setConnectionMode(ElasticConnectionMode connectionMode) {
		this.connectionMode = connectionMode;
	}

	/**
	 * Getter accessor for attribute 'esClient'.
	 *
	 * @return current value of 'esClient'
	 */
	public Client getEsClient() {
		return esClient;
	}

	/**
	 * Setter accessor for attribute 'esClient'.
	 * 
	 * @param esClient
	 *            new value for 'esClient '
	 */
	public void setEsClient(Client esClient) {
		this.esClient = esClient;
	}

	/**
	 * Getter accessor for attribute 'jestClient'.
	 *
	 * @return current value of 'jestClient'
	 */
	public JestClient getJestClient() {
		return jestClient;
	}

	/**
	 * Setter accessor for attribute 'jestClient'.
	 * 
	 * @param jestClient
	 *            new value for 'jestClient '
	 */
	public void setJestClient(JestClient jestClient) {
		this.jestClient = jestClient;
	}

	/**
	 * Getter accessor for attribute 'indexName'.
	 *
	 * @return current value of 'indexName'
	 */
	public String getIndexName() {
		return indexName;
	}

	/**
	 * Setter accessor for attribute 'indexName'.
	 * 
	 * @param indexName
	 *            new value for 'indexName '
	 */
	public void setIndexName(String indexName) {
		this.indexName = indexName;
	}

	/**
	 * Getter accessor for attribute 'urlSet'.
	 *
	 * @return current value of 'urlSet'
	 */
	public Set<URL> getUrlSet() {
		return urlSet;
	}

	/**
	 * Setter accessor for attribute 'urlSet'.
	 * 
	 * @param urlSet
	 *            new value for 'urlSet '
	 */
	public void setUrlSet(Set<URL> urlSet) {
		this.urlSet = urlSet;
	}
}
