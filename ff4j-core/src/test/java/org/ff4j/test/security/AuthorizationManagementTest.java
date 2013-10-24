package org.ff4j.test.security;

/*
 * #%L AuthorizationManagementTest.java (ff4j-core) by Cedrick LUNVEN %% Copyright (C) 2013 Ff4J %% Licensed under the Apache
 * License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.io.IOException;
import java.util.Arrays;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.test.strategy.TestAlwaysTrueFlippingStrategy;
import org.junit.Test;

public class AuthorizationManagementTest {

    @Test
    public void testAllowed() throws IOException {
        // Build Feature
        Feature full = new Feature("abc", true, "Full", Arrays.asList(new String[] {"ROLEA"}));
        full.setFlippingStrategy(new TestAlwaysTrueFlippingStrategy());

        System.out.println(full.isEnable());
        FF4j inMemoryFf4j = new FF4j().create(full).create("def", true);
        inMemoryFf4j.isAllowed(full);

        inMemoryFf4j.setAuthorizationsManager(new TestAuthorizationManager());
        inMemoryFf4j.isAllowed(full);

        if (inMemoryFf4j.isFlipped(full.getUid(), "ctx")) {
            // Random access cannot predict :)

        }

        if (inMemoryFf4j.isFlipped(full.getUid(), full.getFlippingStrategy(), "ctx")) {
            // Random access cannot predict :)
        }

        inMemoryFf4j.toString();
        inMemoryFf4j.exportFeatures();

    }
}
