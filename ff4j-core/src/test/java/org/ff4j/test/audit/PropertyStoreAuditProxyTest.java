package org.ff4j.test.audit;

/*
 * #%L
 * ff4j-core
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

import org.ff4j.FF4j;
import org.ff4j.audit.proxy.PropertyStoreAuditProxy;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.property.AbstractPropertyStoreJunitTest;
import org.junit.Test;

public class PropertyStoreAuditProxyTest extends AbstractPropertyStoreJunitTest {

    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        FF4j ff4j = new FF4j();
        PropertyStore ps = new InMemoryPropertyStore("ff4j.xml");
        ff4j.setPropertiesStore(ps);
        return new PropertyStoreAuditProxy(ff4j, ps);
    }
    
    @Test
    public void testCreateSchema() {
        testedStore.createSchema();
    }

}
