package org.ff4j.utils.json;

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

import com.fasterxml.jackson.databind.ObjectMapper;
import org.ff4j.audit.Event;

import java.io.IOException;
import java.util.*;

/**
 * Unmarshalling event data from JSON using Jackson.
 *
 * @author Curtis White (@drizztguen77)
 */
public class EventJsonParser {

    /**
     * Jackson mapper.
     */
    private static ObjectMapper objectMapper = new ObjectMapper();

    /**
     * Hide constructor.
     */
    private EventJsonParser() {
    }

    /**
     * Unmarshall {@link Event} from json string.
     *
     * @param json json representation of event.
     * @return event object
     */
    @SuppressWarnings("unchecked")
    public static Event parseEvent(String json) {
        try {
            return parseEventMap(objectMapper.readValue(json, HashMap.class));
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot parse json as Event " + json, e);
        }
    }

    private static Event parseEventMap(Map<String, Object> eMap) {
        Event e = new Event();

        e.setUuid((String) eMap.get("id"));
        e.setTimestamp((Long) eMap.get("timestamp"));
        e.setHostName((String) eMap.get("hostName"));
        e.setSource((String) eMap.get("source"));
        e.setUser((String) eMap.get("user"));
        e.setName((String) eMap.get("name"));
        e.setType((String) eMap.get("type"));
        e.setAction((String) eMap.get("action"));
        e.setValue((String) eMap.get("value"));

        if (null != eMap.get("duration")) {
            if (eMap.get("duration") instanceof Integer) {
                e.setDuration(((Integer) eMap.get("duration")).longValue());
            } else {
                e.setDuration((Long) eMap.get("duration"));
            }
        } else {
            e.setDuration(0L);
        }

        // Handle custom keys - Get all keys and remove known keys. The remaining are custom keys
        Set<String> keys = eMap.keySet();
        keys.removeAll(Arrays.asList("id", "timestamp", "hostName", "source", "user", "name", "type", "action", "value", "duration"));
        for (String key : keys) {
            e.getCustomKeys().put(key, (String) eMap.get(key));
        }

        return e;
    }

    /**
     * Convert event array to json.
     *
     * @param events target events
     * @return json string
     */
    public static String eventArrayToJson(Event[] events) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        if (events != null) {
            boolean first = true;
            for (Event event : events) {
                sb.append(first ? "" : ",");
                sb.append(event.toJson());
                first = false;
            }
        }
        sb.append("]");
        return sb.toString();
    }

    /**
     * Parse the json expression as array of {@link Event}.
     *
     * @param json json expression
     * @return array of event
     */
    @SuppressWarnings("unchecked")
    public static Event[] parseEventArray(String json) {
        if (null == json || "".equals(json)) {
            return null;
        }
        try {
            List<LinkedHashMap<String, Object>> evtMap = objectMapper.readValue(json, List.class);
            Event[] eArray = new Event[evtMap.size()];
            int idx = 0;
            for (LinkedHashMap<String, Object> ll : evtMap) {
                eArray[idx++] = parseEventMap(ll);
            }
            return eArray;
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse JSON " + json, e);
        }
    }

}
