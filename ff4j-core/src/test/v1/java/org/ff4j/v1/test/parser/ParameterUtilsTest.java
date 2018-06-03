package org.ff4j.v1.test.parser;

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

import java.lang.reflect.Constructor;

import org.junit.Test;

import MappingUtil;

/**
 * Check Constructor
 *
 * @author Cedrick Lunven (@clunven)
 */
public class ParameterUtilsTest {

    @Test
    public void testConstructorParameterUtils() throws Exception {
        Constructor<MappingUtil> ce = MappingUtil.class.getDeclaredConstructor();
        ce.setAccessible(true);
        ce.newInstance();
    }

}
