package org.ff4j.elastic;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.builder.SearchSourceBuilder;
import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.utils.Util;

import io.searchbox.client.JestResult;
import io.searchbox.core.Delete;
import io.searchbox.core.Index;
import io.searchbox.core.Search;
import io.searchbox.core.SearchResult;
import io.searchbox.core.SearchResult.Hit;
import io.searchbox.core.Update;
import io.searchbox.indices.DeleteIndex;
import io.searchbox.indices.Flush;

/**
 * Helper to create Jest queries.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class ElasticQueryBuilder {
    
    /** Connection. */
    private final ElasticConnection connection;
    
    /**
     * Initialization of the builder with a dedicated connection.
     *
     * @param conn
     *      current elastic collection.
     */
    public ElasticQueryBuilder(ElasticConnection conn) {
        this.connection = conn;
    }
    
    public Flush queryFlushIndex() {
        return new Flush.Builder()
                .addIndex(connection.getIndexName()).build();
    }
    
    /**
     * Syntaxic sugar to have query on feature.
     *
     * @param uid
     *         target feature uid
     * @return
     *         query for JEST
     */
    public Search queryGetFeatureById(String uid) {
        SearchSourceBuilder source = new SearchSourceBuilder();
        source.query(QueryBuilders.matchQuery("uid", uid));
        return new Search.Builder(source.toString())
                .addIndex(connection.getIndexName())
                .addType(ElasticConstants.TYPE_FEATURE).build();
    }
    
    public Search getGroupByGroupName(String groupName) {
        SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
        searchSourceBuilder.query(QueryBuilders.matchQuery("group", groupName));
        return new Search.Builder(searchSourceBuilder.toString()) //
                .addIndex(connection.getIndexName()) //
                .addType(ElasticConstants.TYPE_FEATURE).build();
    }
    
    public Index queryCreateFeature(Feature fp) {
        return new Index.Builder(fp)
                .index(connection.getIndexName())
                .type(ElasticConstants.TYPE_FEATURE)
                .refresh(true).build();
    }
    
    public Search queryReadAll() {
        return new Search.Builder(new SearchSourceBuilder().toString())
                .addIndex(connection.getIndexName())
                .addType(ElasticConstants.TYPE_FEATURE).build();
    }
    
    public Delete queryDeleteFeature(String uid) {
        return new Delete.Builder(uid)
                .index(connection.getIndexName())
                .type(ElasticConstants.TYPE_FEATURE)
                .id(getFeatureTechId(uid))
                .refresh(true)
                .build();
    }
    
    public Index queryUpdateFeature(Feature fp) {
        return new Index.Builder(fp)
                .index(connection.getIndexName())
                .type(ElasticConstants.TYPE_FEATURE)
                .id(getFeatureTechId(fp.getUid()))
                .refresh(true)
                .build();
    }
    
    public String getFeatureTechId(String uid) {
        SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
        searchSourceBuilder.query(QueryBuilders.matchQuery("uid", uid));
        Search search = new Search.Builder(searchSourceBuilder.toString()) //
                .addIndex(connection.getIndexName()) //
                .addType(ElasticConstants.TYPE_FEATURE) //
                .build();
        // feature existence must have been cheked before (technical function)
        List<Hit<Map, Void>> items = connection.search(search).getHits(Map.class);
        if (null != items && !items.isEmpty()) {
            return connection.search(search).getHits(Map.class).get(0)
                .source.get(JestResult.ES_METADATA_ID).toString();
        }
        return null;
    }
    
    @SuppressWarnings({"rawtypes"})
    public Set<String> getFeatureTechIdByGroup(String groupName) {
        SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
        searchSourceBuilder.query(QueryBuilders.matchQuery("group", groupName));
        Search search = new Search.Builder(searchSourceBuilder.toString()) //
                    .addIndex(connection.getIndexName()) //
                    .addType(ElasticConstants.TYPE_FEATURE) //
                    .build();
            
        SearchResult result = connection.search(search, true);
        Set<String> metadatas = new HashSet<String>();
        if (null != result && result.isSucceeded()) {
            List<Hit<Map, Void>> features = result.getHits(Map.class);
            for (Hit<Map, Void> hit : features) {
                metadatas.add(hit.source.get(JestResult.ES_METADATA_ID).toString());
            }
        }
        return metadatas;
    }
    
    /**
     * Update status of feature.
     * 
     * @param uid
     *            feature id
     * @param enable
     *            enabler
     */
    public Update updateStatus(String uid, boolean enable) {
        String partialDoc = "{ \"doc\" : { \"enable\" : " + enable + " } }";
        return new Update.Builder(partialDoc) //
                .index(connection.getIndexName()) //
                .type(ElasticConstants.TYPE_FEATURE) //
                .id(getFeatureTechId(uid)) //
                .refresh(true) //
                .build();
    }
    
    public  Update queryUpdateStatusWithTechId(String _id, boolean enable) {
        String partialDoc = "{ \"doc\" : { \"enable\" : " + enable + " } }";
        return new Update.Builder(partialDoc) //
                .index(connection.getIndexName()) //
                .type("feature") //
                .id(_id) //
                .build();
    }
    
    public Update queryEnableWithTechId(String _id) {
        return queryUpdateStatusWithTechId(_id, true);
    }
    
    public Update queryDisableWithTechId(String _id) {
        return queryUpdateStatusWithTechId(_id, false);
    }
    
    public Update queryEnable(String uid) {
        return updateStatus(uid, true);
    }
    
    public Update queryDisable(String uid) {
        return updateStatus(uid, false);
    }

    public DeleteIndex queryClear() {
        return new DeleteIndex.Builder(connection.getIndexName()).build();
    }
}
