package org.ff4j.hazelcast.store;

/*
 * #%L
 * ff4j-store-hazelcast
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

import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.BeforeClass;

/**
 * Test to work with Redis as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyStoreHazelCastTest extends PropertyStoreTestSupport {


    private static PropertyStoreHazelCast propertyStoreHazelCast;

    @BeforeClass
    public static void setupHazelcast(){
        propertyStoreHazelCast = new PropertyStoreHazelCast();
        propertyStoreHazelCast.importPropertiesFromXmlFile("ff4j.xml");
    }

   
    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        return propertyStoreHazelCast;
    }
    
}
