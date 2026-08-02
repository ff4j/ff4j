package org.ff4j.test.security;

/*-
 * #%L
 * ff4j-test
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


import org.ff4j.utils.Util;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DefaultAuthorisationTest {
    
    @Test
    public void testDefaultSecurityManager() {
        DefaultAuthorisationManager am = new DefaultAuthorisationManager();
        am.setAllPermissions(Util.set("a","b","c"));
        am.setCurrentUserPermissions(Util.set("b","c"));
        Assertions.assertNotNull(am.getCurrentUserPermissions());
        Assertions.assertNotNull(am.listAllPermissions());   
    }
    
    @Test
    public void testDefaultSecurityManagerBis() {
        DefaultAuthorisationManager am = new DefaultAuthorisationManager(Util.set("b","c"), Util.set("a","b","c"));
        Assertions.assertNotNull(am.getCurrentUserPermissions());
        Assertions.assertNotNull(am.listAllPermissions());
        Assertions.assertNotNull(am.toJson());
    }

}
