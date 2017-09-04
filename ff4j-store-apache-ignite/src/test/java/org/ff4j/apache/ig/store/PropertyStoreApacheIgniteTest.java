package org.ff4j.apache.ig.store;

/*
 * #%L
 * ff4j-store-apache-ignite
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

import org.apache.ignite.Ignite;
import org.apache.ignite.Ignition;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;

public class PropertyStoreApacheIgniteTest extends PropertyStoreTestSupport {

    private static Ignite ignite;

    @BeforeClass
    public static void setupApacheIgnite() {
        ignite = Ignition.start();
    }

    @AfterClass
    public static void shutDonwApacheIgnite() {
        ignite.close();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreApacheIgnite ff4j = new PropertyStoreApacheIgnite(ignite.getOrCreateCache("ff4j"));
        ff4j.importPropertiesFromXmlFile("ff4j.xml");
        return ff4j;
    }

}
