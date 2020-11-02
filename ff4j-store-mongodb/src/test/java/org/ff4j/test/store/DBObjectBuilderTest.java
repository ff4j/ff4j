package org.ff4j.test.store;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventConstants;

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


import org.ff4j.mongo.mapper.FeatureDocumentBuilder;
import org.ff4j.mongo.mapper.MongoEventMapper;
import org.junit.Assert;
import org.junit.Test;

public class DBObjectBuilderTest {
    
    @Test
    public void tesDBBuilder() {
        FeatureDocumentBuilder db = new FeatureDocumentBuilder();
        Assert.assertNotNull(db.getExpression("Value"));
        Assert.assertNotNull(db.getStrategy("Value"));
        Assert.assertNotNull(db.getDescription("Value"));
    }
    
    @Test
    public void testMongoEventMapper() {
        Event evt = new Event("JAVA", EventConstants.TARGET_FEATURE, 
                "toto", EventConstants.ACTION_CHECK_OFF);
        MongoEventMapper mem = new MongoEventMapper();
        Event evt2 = mem.fromStore(mem.toStore(evt));
        Assert.assertEquals(evt.getUuid(), evt2.getUuid());
        
    }

}
