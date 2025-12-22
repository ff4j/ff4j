package org.ff4j.test.store;

/*-
 * #%L
 * ff4j-core
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

import static org.junit.jupiter.api.Assertions.assertThrows;

/*-
 * #%L
 * ff4j-core
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

import java.util.LinkedHashMap;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.PonderationStrategy;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * All TEST LOGIC is in super class to be processed on EACH STORE.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class InMemoryFeatureStoreTest extends CoreFeatureStoreTestSupport {

    /** {@inheritDoc} */
    @Override
    public FeatureStore initStore() {
        InMemoryFeatureStore imfs = new InMemoryFeatureStore();
        imfs.setLocation("ff4j.xml");
        return imfs;
    }

    @Test
    public void testUnitFeatureInitialization() {
        InMemoryFeatureStore imfs = new InMemoryFeatureStore();
        imfs.create(new Feature("default", true, "grp1", "desc", null, new PonderationStrategy()));
        Assertions.assertEquals(1, imfs.readAll().size());
    }

    @Test
    public void testUnitFeatureInitialization2() {
        LinkedHashMap<String, Feature> map1 = new LinkedHashMap<String, Feature>();
        map1.put("new", new Feature("new", true, "description"));
        map1.put("old", new Feature("old", true, "description"));
        InMemoryFeatureStore imfs = new InMemoryFeatureStore(map1);
        Assertions.assertEquals(2, imfs.readAll().size());
        Assertions.assertNotNull(imfs.read("old"));
    }

    @Test
    public void testUnitFeatureInitialization3() {
        assertThrows(IllegalArgumentException.class, () -> {
            new InMemoryFeatureStore("invalid.xml");
        });
    }
    
    @Test
    public void testUnitFeatureInitialization5() {
        assertThrows(IllegalArgumentException.class, () -> {
            new InMemoryFeatureStore((String) null);
        });
    }
    
    @Test
    public void testUnitFeatureInitialization6() {
        assertThrows(IllegalArgumentException.class, () -> {
            new InMemoryFeatureStore("");
        });
    }
    
    @Test
    public void testUnitFeatureInitialization4() {
        InMemoryFeatureStore f = new InMemoryFeatureStore();
        f.toJson();
        f.toString();
        f.getFileName();
    }
    
    @Test
    public void testDonotImportEmpty() {
        assertThrows(IllegalArgumentException.class, () -> {
            InMemoryFeatureStore f = new InMemoryFeatureStore();
            f.importFeaturesFromXmlFile("");
        });
    }
    
    
    @Test
    public void testDonotImportNull() {
        assertThrows(IllegalArgumentException.class, () -> {
            InMemoryFeatureStore f = new InMemoryFeatureStore();
            f.importFeaturesFromXmlFile(null);
        });
    }
    
    @Test
    public void testDonotImportInvalid() {
        assertThrows(IllegalArgumentException.class, () -> {
            InMemoryFeatureStore f = new InMemoryFeatureStore();
            f.importFeaturesFromXmlFile("invalid.xml");
        });
    }
    
    @Test
    public void testImportTwice() {
        InMemoryFeatureStore f = new InMemoryFeatureStore();
        f.importFeaturesFromXmlFile("ff4j.xml");
        f.importFeaturesFromXmlFile("ff4j.xml");
        Assertions.assertFalse(f.readAll().isEmpty());
    }
    
}
