package org.ff4j.test.utils;

import java.io.File;

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

import java.io.IOException;
import java.lang.reflect.Constructor;

import org.ff4j.FF4j;
import org.ff4j.utils.GeneratorUtils;
import org.junit.Assert;
import org.junit.Test;

public class GenerationUtilTest {
    
    @Test
    public void instanciate() throws Exception {
        Constructor<GeneratorUtils> ce = GeneratorUtils.class.getDeclaredConstructor();
        ce.setAccessible(true);
        ce.newInstance();
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testNull()
    throws IOException {
        GeneratorUtils.generateInterfaceConstantsSource(null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testNull2()
    throws IOException {
        GeneratorUtils.generateInterfaceConstantFile(new FF4j("ff4j.xml"), null);
    }
    
    @Test
    public void generationSource()
    throws IOException {
        // Given
        FF4j ff4j = new FF4j("ff4j.xml");
        Assert.assertNotNull(ff4j.getFeatureStore());
        Assert.assertNotNull(ff4j.getPropertiesStore());
        // When
        String data = GeneratorUtils.generateInterfaceConstantsSource(ff4j);
        // Then
        Assert.assertTrue(data.contains("FEATURE"));
        Assert.assertTrue(data.contains("interface"));
        Assert.assertNotNull(GeneratorUtils.exportInterfaceConstants(ff4j));
        GeneratorUtils.generateInterfaceConstantFile(ff4j, new File("./target"));
    }    
    
    
}
