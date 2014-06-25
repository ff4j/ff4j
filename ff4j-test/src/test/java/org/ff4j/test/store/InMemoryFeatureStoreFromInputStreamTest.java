package org.ff4j.test.store;

/*
 * #%L ff4j-core $Id:$ $HeadURL:$ %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import org.ff4j.core.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Assert;
import org.junit.Test;

import java.io.InputStream;

/**
 * All TEST LOGIC is in super class to be processed on EACH STORE.
 * 
 * @author joemoore
 */
public class InMemoryFeatureStoreFromInputStreamTest extends AbstractStoreJUnitTest {

    /** {@inheritDoc} */
    @Override
    public FeatureStore initStore() {
        return inMemoryFeatureStoreFromInputStream;
    }

    /**
     * TDD.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testUnitFeatureInitialization3() {
        // Given
        // input stream is null
      new InMemoryFeatureStore((InputStream) null);
    }

    @Test
    public void fileNameUnusedAndNull() {
      Assert.assertNull(((InMemoryFeatureStore)inMemoryFeatureStoreFromInputStream).getFileName());
    }
}
