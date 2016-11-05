package org.ff4j.elastic.server;

import java.util.Optional;

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
import org.elasticsearch.common.settings.Settings;
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
			instance.clusterName = Optional.ofNullable(name).orElse("elastic");
			return this;
		}

		public Builder dataDirectory(String path) {
			instance.path = Optional.ofNullable(path).orElse("data");
			return this;
		}

		public Builder health(boolean health) {
			instance.health = health;
			return this;
		}

		public Node start() {

			// Configure before starting the instance
			Settings elasticSettings = Settings.settingsBuilder() //
					.put("path.data", instance.path) //
					.put("path.home", "/") //
					.build();

			Node node = NodeBuilder.nodeBuilder().clusterName(instance.clusterName) //
					.local(true).settings(elasticSettings) //
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
