package org.ff4j.elastic.store;

/*-
 * #%L
 * ff4j-store-elastic
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

import java.io.IOException;

import org.ff4j.audit.repository.EventRepository;
import org.ff4j.elastic.ElasticQueryBuilder;
import org.ff4j.test.audit.EventRepositoryTestSupport;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Implementation of Unit Test for event repository.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@Ignore
public class EventRepositoryElasticTest extends EventRepositoryTestSupport {
	
    /** {@inheritDoc} */
	@Override
	protected EventRepository initRepository() {
	    EventRepositoryElastic ese = new EventRepositoryElastic(
	            JestClientTestFactory.getJestClient(), 
                EventRepositoryElastic.DEFAULT_INDEX_EVENT);
        ese.createSchema();
	    try {
	        JestClientTestFactory
	            .getJestClient()
	            .execute(ElasticQueryBuilder.deleteAllEvents(ese.getIndexEvents()));
        } catch (IOException e) {
           throw new IllegalArgumentException("Cannot empty the index");
        }
	    return ese;
	}
	
	@Test
	public void testPurgeEvents() 
	throws InterruptedException {}
}
