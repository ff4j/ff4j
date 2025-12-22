package org.ff4j.test.utils;

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

import org.ff4j.utils.IOUtil;
import org.ff4j.utils.Util;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class IOUtilsTest {
    
    @Test
    public void testInit() throws Exception {
        Assertions.assertNotNull(Util.instanciatePrivate(IOUtil.class));
    }
    
    @Test
    public void testResolveOK() throws Exception {
        IOUtil.setUseInetAddress(true);
        IOUtil.resolveHostName();
        Assertions.assertTrue(IOUtil.isUseInetAddress());
    }
    
    @Test
    public void testResolveKO() throws Exception {
        assertThrows(IllegalArgumentException.class, () -> {
            IOUtil.setUseInetAddress(false);
            IOUtil.resolveHostName();
            IOUtil.setUseInetAddress(true);
            Assertions.fail();
        });
    }

}
