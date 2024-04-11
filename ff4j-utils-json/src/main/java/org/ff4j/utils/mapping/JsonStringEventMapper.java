package org.ff4j.utils.mapping;

/*-
 * #%L
 * ff4j-utils-json
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import org.ff4j.audit.Event;
import org.ff4j.mapper.EventMapper;
import org.ff4j.utils.json.EventJsonParser;

/**
 * Implementation to map {@link Event} to Json String and vice-versa
 *
 * @author Curtis White (@drizztguen77)
 */
public class JsonStringEventMapper implements EventMapper<String> {

    /**
     * {@inheritDoc}
     */
    @Override
    public String toStore(Event bean) {
        if (bean == null) return null;
        return bean.toJson();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Event fromStore(String bean) {
        return EventJsonParser.parseEvent(bean);
    }

}
