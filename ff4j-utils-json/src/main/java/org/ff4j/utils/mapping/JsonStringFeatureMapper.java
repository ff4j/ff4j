package org.ff4j.utils.mapping;

import org.ff4j.feature.Feature;
import org.ff4j.mapper.FeatureMapper;
import org.ff4j.utils.json.FeatureJsonParser;

/**
 * Implementation to map {@link Feature} to Json String and vice-versa
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class JsonStringFeatureMapper implements FeatureMapper < String, String > {

    /** {@inheritDoc} */
    @Override
    public String mapToRepository(Feature bean) {
        if (bean == null) return null;
        return bean.toJson();
    }

    /** {@inheritDoc} */
    @Override
    public Feature mapFromRepository(String bean) {
        return FeatureJsonParser.parseFeature(bean);
    }

}
