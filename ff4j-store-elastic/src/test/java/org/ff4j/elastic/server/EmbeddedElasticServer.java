package org.ff4j.elastic.server;

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


import org.elasticsearch.action.admin.cluster.health.ClusterHealthResponse;
import org.elasticsearch.common.settings.ImmutableSettings;
import org.elasticsearch.node.Node;
import org.elasticsearch.node.NodeBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author <a href="mailto:andre.blaszczyk@gmail.com">Andre Blaszczyk</a>
 *
 */
public class EmbeddedElasticServer {

	private final static Logger logger = LoggerFactory.getLogger(EmbeddedElasticServer.class);

	private String clusterName;
	private String path;
	private boolean health;

	private EmbeddedElasticServer() {
	}

	public static Builder builder() {
		return new EmbeddedElasticServer.Builder();
	}

	public static class Builder {

		private EmbeddedElasticServer instance = new EmbeddedElasticServer();

		public Builder() {
		}

		public Builder clusterName(String name) {
			instance.clusterName = (name != null ? name : "elastic");
			return this;
		}

		public Builder dataDirectory(String path) {
			instance.path = (path != null ? path : "data");
			return this;
		}

		public Builder health(boolean health) {
			instance.health = health;
			return this;
		}

		public Node start() {

			// Configure before starting the instance
			ImmutableSettings.Builder elasticSettings = ImmutableSettings.settingsBuilder() //
					.put("path.data", instance.path);

			Node node = NodeBuilder.nodeBuilder().clusterName(instance.clusterName) //
					.local(true).settings(elasticSettings.build()) //
					.node();

			node.start();

			// Display cluster information
			if (instance.health) {
				ClusterHealthResponse health = node.client().admin().cluster().prepareHealth().get();
				logger.info(health.toString());
			}

			return node;
		}
	}
}
