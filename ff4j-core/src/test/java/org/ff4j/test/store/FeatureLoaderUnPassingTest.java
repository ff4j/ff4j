package org.ff4j.test.store;

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

import java.io.InputStream;

import javax.xml.parsers.ParserConfigurationException;

import org.ff4j.core.FeatureLoader;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

@RunWith(PowerMockRunner.class)
@PrepareForTest(FeatureLoader.class)
public class FeatureLoaderUnPassingTest {

    @SuppressWarnings("unchecked")
    @Test
    @Ignore
    public void mockStaticFeatureLoader() throws Exception {

        // mock all the static methods in a class called "Static"
        PowerMockito.mockStatic(FeatureLoader.class);
        
        // use Mockito to set up your expectation
        Mockito.when(FeatureLoader.getDocumentBuilder()).thenThrow(ParserConfigurationException.class);
        
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j.xml");
        FeatureLoader.loadFeatures(in);
        PowerMockito.verifyStatic();

    }

}