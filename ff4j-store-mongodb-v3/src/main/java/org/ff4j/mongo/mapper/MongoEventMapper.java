package org.ff4j.mongo.mapper;

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
        evt.setDuration(bean.getLong(ATTRIBUTE_DURATION));
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
