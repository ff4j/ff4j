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

import java.io.File;

import java.io.IOException;
import java.lang.reflect.Constructor;

import org.ff4j.FF4j;
import org.ff4j.conf.XmlParser;
import org.ff4j.utils.GeneratorUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class GenerationUtilTest {
    
    @Test
    public void instanciate() throws Exception {
        Constructor<GeneratorUtils> ce = GeneratorUtils.class.getDeclaredConstructor();
        ce.setAccessible(true);
        ce.newInstance();
    }
    
    @Test
    public void testNull()
    throws IOException {
        assertThrows(IllegalArgumentException.class, () ->
            GeneratorUtils.generateInterfaceConstantsSource(null));
    }
    
    @Test
    public void testNull2()
    throws IOException {
        assertThrows(IllegalArgumentException.class, () ->
            GeneratorUtils.generateInterfaceConstantFile(new FF4j(new XmlParser(), "ff4j.xml"), null));
    }
    
    @Test
    public void generationSource()
    throws IOException {
        // Given
        FF4j ff4j = new FF4j(new XmlParser(),"ff4j.xml");
        Assertions.assertNotNull(ff4j.getFeatureStore());
        Assertions.assertNotNull(ff4j.getPropertiesStore());
        // When
        String data = GeneratorUtils.generateInterfaceConstantsSource(ff4j);
        // Then
        Assertions.assertTrue(data.contains("FEATURE"));
        Assertions.assertTrue(data.contains("interface"));
        Assertions.assertNotNull(GeneratorUtils.exportInterfaceConstants(ff4j));
        GeneratorUtils.generateInterfaceConstantFile(ff4j, new File("./target"));
    }    
    
    
}
