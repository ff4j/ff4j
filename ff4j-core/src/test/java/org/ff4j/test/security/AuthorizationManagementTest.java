package org.ff4j.test.security;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
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
import java.util.Arrays;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.security.AuthorizationsManager;
import org.ff4j.test.AbstractFf4jTest;
import org.junit.Assert;
import org.junit.Test;


/**
 * Class to test component {@link AuthorizationsManager}
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class AuthorizationManagementTest extends AbstractFf4jTest {

    /** {@inheritDoc} */
    @Override
    public FF4j initFF4j() {
        FF4j ff4j = new FF4j();
        ff4j.setAuthorizationsManager(mockAuthManager);
        return ff4j;
    }

    @Test
    public void testAllowed() throws IOException {
        Feature ok = new Feature("ok", true, "Full1", "GRP1", Arrays.asList(new String[] {"ROLEA"}));
        ff4j.createFeature(ok);
        assertFf4j.assertThatCurrentUserIsAllowedOnFeature(ok.getUid());
    }

    @Test
    public void testNotAllowed() {
        Feature ko = new Feature("ko", true, "Full2", "GRP2", Arrays.asList(new String[] {"ROLEC"}));
        ff4j.createFeature(ko);
        assertFf4j.assertThatCurrentUserIsNotAllowedOnFeature(ko.getUid());
    }

    @Test
    public void testAllowedNoManager() throws IOException {
        ff4j.createFeature("new", true);
        ff4j.setAuthorizationsManager(null);
        assertFf4j.assertThatCurrentUserIsAllowedOnFeature("new");
    }
    
    @Test
    public void testToJson() {
        // Given
        Assert.assertNotNull(ff4j);
        Assert.assertNotNull(ff4j.getAuthorizationsManager());
        // When
        String jsonExpr = ff4j.getAuthorizationsManager().toJson();
        // Then
        Assert.assertNotNull(jsonExpr);
    }

}
