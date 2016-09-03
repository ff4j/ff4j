package org.ff4j.test.utils;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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
import org.junit.Assert;
import org.junit.Test;

public class IOUtilsTest {
    
    @Test
    public void testInit() throws Exception {
        Assert.assertNotNull(Util.instanciatePrivate(IOUtil.class));
    }
    
    @Test
    public void testResolveOK() throws Exception {
        IOUtil.setUseInetAddress(true);
        IOUtil.resolveHostName();
        Assert.assertTrue(IOUtil.isUseInetAddress());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testResolveKO() throws Exception {
        IOUtil.setUseInetAddress(false);
        IOUtil.resolveHostName();
        IOUtil.setUseInetAddress(true);
        Assert.fail();
    }

}
