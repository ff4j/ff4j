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
import org.ff4j.utils.Util;

import io.searchbox.client.JestClient;
import io.searchbox.client.JestClientFactory;
import io.searchbox.client.config.HttpClientConfig;
import io.searchbox.indices.CreateIndex;
import io.searchbox.indices.DeleteIndex;
import io.searchbox.indices.IndicesExists;

/**
 * Poor design by clun, to be challenged !!
 */
public class ElasticConnection {
   
    private ElasticConnectionMode connectionMode;
    
    private Client esClient;
    
    private JestClient jestClient;
    
    private String indexName;
    
    private Set < URL > urlSet;
    
    /**
     * Proposal to initiate a connection to a elastic cluster.
     *
     * @param mode
     *      choose client to be used
     * @param url
     *      target nodes of the cluster (optional)
     */
    public ElasticConnection(ElasticConnectionMode mode, String indexName, URL...url) {
        Util.assertParamHasNotNull(mode, "ElasticConnectionMode");
        Util.assertParamHasNotNull(indexName, "indexName");
        this.indexName      = indexName;
        this.connectionMode = mode;
        this.urlSet         = Util.set(url);
        
        try {
            switch (mode) {
                case NATIVE_CLIENT    :   initNativeClient();    break;
                case TRANSPORT_CLIENT :   initTransportClient(); break;
                default               :   initJestClient();
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
        boolean indexExists = esClient.admin().indices().
                prepareExists(indexName).execute().actionGet().isExists();
        if (indexExists) {
            esClient.admin().indices()
            .prepareDelete(indexName).execute()
            .actionGet();
        }
        esClient.admin().indices()
        .prepareCreate(indexName).execute().actionGet();
    }
    
    private void initJestClient() throws IOException {
        JestClientFactory factory = new JestClientFactory();
        factory.setHttpClientConfig(new HttpClientConfig.Builder(mapUrl()).multiThreaded(true).build());
        jestClient = factory.getObject();
        boolean indexExists = jestClient.execute(new IndicesExists.Builder(indexName).build()).isSucceeded();
        if (indexExists) {
            jestClient.execute(new DeleteIndex.Builder(indexName).build());
        }
        jestClient.execute(new CreateIndex.Builder(indexName).build());
    }
    
    /**
     * Before Lambda...
     */
    private Collection < String > mapUrl() {
        Collection<String> collec = new HashSet<String>();
        for (URL url : urlSet) {
            collec.add(url.toString());
        }
        return collec;
    }

    /**
     * Getter accessor for attribute 'connectionMode'.
     *
     * @return
     *       current value of 'connectionMode'
     */
    public ElasticConnectionMode getConnectionMode() {
        return connectionMode;
    }

    /**
     * Setter accessor for attribute 'connectionMode'.
     * @param connectionMode
     * 		new value for 'connectionMode '
     */
    public void setConnectionMode(ElasticConnectionMode connectionMode) {
        this.connectionMode = connectionMode;
    }

    /**
     * Getter accessor for attribute 'esClient'.
     *
     * @return
     *       current value of 'esClient'
     */
    public Client getEsClient() {
        return esClient;
    }

    /**
     * Setter accessor for attribute 'esClient'.
     * @param esClient
     * 		new value for 'esClient '
     */
    public void setEsClient(Client esClient) {
        this.esClient = esClient;
    }

    /**
     * Getter accessor for attribute 'jestClient'.
     *
     * @return
     *       current value of 'jestClient'
     */
    public JestClient getJestClient() {
        return jestClient;
    }

    /**
     * Setter accessor for attribute 'jestClient'.
     * @param jestClient
     * 		new value for 'jestClient '
     */
    public void setJestClient(JestClient jestClient) {
        this.jestClient = jestClient;
    }

    /**
     * Getter accessor for attribute 'indexName'.
     *
     * @return
     *       current value of 'indexName'
     */
    public String getIndexName() {
        return indexName;
    }

    /**
     * Setter accessor for attribute 'indexName'.
     * @param indexName
     * 		new value for 'indexName '
     */
    public void setIndexName(String indexName) {
        this.indexName = indexName;
    }

    /**
     * Getter accessor for attribute 'urlSet'.
     *
     * @return
     *       current value of 'urlSet'
     */
    public Set<URL> getUrlSet() {
        return urlSet;
    }

    /**
     * Setter accessor for attribute 'urlSet'.
     * @param urlSet
     * 		new value for 'urlSet '
     */
    public void setUrlSet(Set<URL> urlSet) {
        this.urlSet = urlSet;
    }
}
