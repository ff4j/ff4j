package org.ff4j.utils.mapping;

import org.ff4j.mapper.PropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.utils.json.PropertyJsonParser;

/**
 * Implementation to map {@link Feature} to Json String and vice-versa.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class JsonStringPropertyMapper implements PropertyMapper<String, String > {

    /** {@inheritDoc} */
    @Override
    public String mapToRepository(Property<?> bean) {
        if (bean == null) return null;
        return bean.toJson();
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> mapFromRepository(String json) {
        return PropertyJsonParser.parseJsonProperty(json);
    }

}
