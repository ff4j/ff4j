package org.ff4j.utils.mapping;

/*
 * #%L
 * ff4j-utils-json
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import org.ff4j.core.Feature;
import org.ff4j.mapper.FeatureMapper;
import org.ff4j.utils.json.FeatureJsonParser;

/**
 * Implementation to map {@link Feature} to Json String and vice-versa
 * @author Cedrick LUNVEN (@clunven)
 */
public class JsonStringFeatureMapper implements FeatureMapper < String > {

    /** {@inheritDoc} */
    @Override
    public String toStore(Feature bean) {
        if (bean == null) return null;
        return bean.toJson();
    }

    /** {@inheritDoc} */
    @Override
    public Feature fromStore(String bean) {
        return FeatureJsonParser.parseFeature(bean);
    }

}
