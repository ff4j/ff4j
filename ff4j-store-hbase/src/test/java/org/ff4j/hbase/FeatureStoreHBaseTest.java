package org.ff4j.hbase;

/*
 * #%L
 * ff4j-store-hbase
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


import org.ff4j.core.FeatureStore;
import org.ff4j.hbase.store.FeatureStoreHBase;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.Ignore;

/**
 * Unit test of {@link FeatureStore} for HBASE.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@Ignore
public class FeatureStoreHBaseTest extends FeatureStoreTestSupport {
    
    /** HBASE_HOST. */
    private static final String HBASE_HOST = "hbase";
    
    /** HBASE_PORT. */
    private static final int HBASE_PORT = 2181;
    
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        FeatureStoreHBase fs = new FeatureStoreHBase(new HBaseConnection(HBASE_HOST, HBASE_PORT, false));
        fs.importFeaturesFromXmlFile("ff4j.xml");
        return fs;
    } 
    
}
