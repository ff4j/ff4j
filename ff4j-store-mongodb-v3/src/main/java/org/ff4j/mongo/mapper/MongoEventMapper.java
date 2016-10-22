package org.ff4j.mongo.mapper;

/*
 * #%L
 * ff4j-store-mongodb-v3
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

import static org.ff4j.audit.EventConstants.ATTRIBUTE_ACTION;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_DURATION;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_HOST;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_ID;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_KEYS;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_NAME;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_SOURCE;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_TIME;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_TYPE;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_USER;

import java.util.Map;

import org.bson.Document;
import org.ff4j.audit.Event;
import org.ff4j.mapper.EventMapper;

import com.mongodb.util.JSON;

/**
 * Implementation of mapper for events.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class MongoEventMapper implements EventMapper< Document > {

    /** {@inheritDoc} */
    @Override
    public Document toStore(Event bean) {
        return Document.parse(bean.toJson());
    }

    /** {@inheritDoc} */
    @SuppressWarnings({"unchecked"})
    @Override
    public Event fromStore(Document bean) {
        Event evt = new Event();
        evt.setAction(bean.getString(ATTRIBUTE_ACTION));
        evt.setDuration(bean.getInteger(ATTRIBUTE_DURATION));
        evt.setHostName(bean.getString(ATTRIBUTE_HOST));
        evt.setName(bean.getString(ATTRIBUTE_NAME));
        evt.setSource(bean.getString(ATTRIBUTE_SOURCE));
        evt.setTimestamp(bean.getLong(ATTRIBUTE_TIME));
        evt.setType(bean.getString(ATTRIBUTE_TYPE));
        evt.setUuid(bean.getString(ATTRIBUTE_ID));
        evt.setUser(bean.getString(ATTRIBUTE_USER));
        if (bean.containsKey(ATTRIBUTE_KEYS)) {
            evt.setCustomKeys((Map<String, String>) 
                    JSON.parse(bean.getString(ATTRIBUTE_KEYS)));
        }
        return evt;
    }

}
