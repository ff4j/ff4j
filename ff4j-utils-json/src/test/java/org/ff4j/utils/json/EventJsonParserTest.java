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

import org.ff4j.FF4j;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventBuilder;
import org.ff4j.conf.XmlParser;
import org.ff4j.security.AuthorizationsManager;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class EventJsonParserTest {

    /**
     * Sample in MemoryStore.
     */
    private final FF4j ff4j = new FF4j(new XmlParser(), "test-ff4j-parser.xml");

    @Test
    public void testMarshaling() {

        ff4j.setAuthorizationsManager(new AuthorizationsManager() {
            public String toJson() {
                return "dummy";
            }

            public Set<String> listAllPermissions() {
                return new HashSet<>();
            }

            public Set<String> getCurrentUserPermissions() {
                return new HashSet<>();
            }

            public String getCurrentUserName() {
                return "dummy";
            }
        });

        Map<String, Event> events = new HashMap<>();
        events.put("e1", new EventBuilder(ff4j).name("f1").build());
        events.put("e2", new EventBuilder(ff4j).name("f2").build());

        for (String key : events.keySet()) {
            // Check serialised
            Event e1 = EventJsonParser.parseEvent(events.get(key).toJson());
            Assert.assertEquals(events.get(key).getName(), e1.getName());
            Assert.assertTrue(events.get(key).compareTo(e1) == 0);
        }
    }

    @Test
    public void testArrays() {

        ff4j.setAuthorizationsManager(new AuthorizationsManager() {
            public String toJson() {
                return "dummy";
            }

            public Set<String> listAllPermissions() {
                return new HashSet<>();
            }

            public Set<String> getCurrentUserPermissions() {
                return new HashSet<>();
            }

            public String getCurrentUserName() {
                return "dummy";
            }
        });

        Map<String, Event> events = new HashMap<>();
        events.put("e1", new EventBuilder(ff4j).name("f1").build());
        events.put("e2", new EventBuilder(ff4j).name("f2").build());

        int idx = 0;
        Event[] e = new Event[events.size()];
        for (String event : events.keySet()) {
            e[idx] = events.get(event);
            idx++;
        }

        String eventsArrayAsJson = EventJsonParser.eventArrayToJson(e);
        Event[] ee = EventJsonParser.parseEventArray(eventsArrayAsJson);
        Assert.assertEquals(events.size(), ee.length);
    }

    @Test
    public void testInit() throws Exception {
        Assert.assertNotNull(Util.instanciatePrivate(EventJsonParser.class));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testInvalidJsonGetIllegalArgument() {
        EventJsonParser.parseEvent("something:invald");
    }

    @Test
    public void testSerialisation() {
        Event event1 = new Event();
        event1.setUuid("e1");
        Event event2 = new Event();
        event2.setUuid("e2");

        Event[] events = {event1, event2};
        Assert.assertNotNull(EventJsonParser.eventArrayToJson(events));
        Assert.assertNotNull(EventJsonParser.eventArrayToJson(null));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testparseEventArrayError() {
        EventJsonParser.parseEventArray("something:invalid");
    }


    @Test
    public void testparseFeatureArrayEmpty() {
        Assert.assertNull(EventJsonParser.parseEventArray(null));
        Assert.assertNull(EventJsonParser.parseEventArray(""));
    }
}
