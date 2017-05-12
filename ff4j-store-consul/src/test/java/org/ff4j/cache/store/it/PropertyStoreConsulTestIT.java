package org.ff4j.cache.store.it;

/*
 * #%L
 * ff4j-store-consul
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

import org.ff4j.consul.ConsulConnection;
import org.ff4j.consul.store.PropertyStoreConsul;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Ignore;

/**
 * Expect to run tests if a consul instance id UP.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@Ignore
public class PropertyStoreConsulTestIT extends PropertyStoreTestSupport {

    /** {@inheritDoc} */
    protected PropertyStore initPropertyStore() {
        ConsulConnection  connection = new ConsulConnection();
        PropertyStoreConsul consulStore = new PropertyStoreConsul(connection);
        consulStore.importPropertiesFromXmlFile("ff4j.xml");
        return consulStore;
    }

}
