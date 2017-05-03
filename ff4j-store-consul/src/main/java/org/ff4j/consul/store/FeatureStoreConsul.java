package org.ff4j.consul.store;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.ff4j.consul.ConsulConnection;
import org.ff4j.consul.ConsulConstants;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.store.kv.KeyValueFeatureStore;
import org.ff4j.utils.json.FeatureJsonParser;

/**
 * Generic {@link FeatureStore} to persist properties in a JCache (JSR107) compliant storage.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreConsul extends KeyValueFeatureStore < ConsulConnection > {
    
    /**
     * Default contructor.
     */
    public FeatureStoreConsul() {
        super();
    }
    
    /**
     * Initialization with cache manager.
     *
     * @param cacheManager
     */
    public FeatureStoreConsul(ConsulConnection connection) {
        super(connection);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        return getDriver().getKeyValueClient()
                .getValuesAsString(ConsulConstants.FF4J_PREFIXKEY_FEATURES)
                .stream()
                .map(FeatureJsonParser::parseFeature)
                .collect(Collectors.toMap(Feature::getUid, Function.identity()));
    }    
    
    /** {@inheritDoc} */
    @Override
    public void clear() {
        getDriver().getKeyValueClient()
                .deleteKeys(ConsulConstants.FF4J_PREFIXKEY_FEATURES);
    }

    
}
