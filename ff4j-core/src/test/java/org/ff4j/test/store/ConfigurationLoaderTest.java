package org.ff4j.test.store;

/*
 * #%L
 * ff4j-core
 * $Id:$
 * $HeadURL:$
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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.LinkedHashMap;
import java.util.Map;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureLoader;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Test;

/**
 * Unit Testing
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ConfigurationLoaderTest extends TestCase {

    @Test
    public void testSaxException() {
        try {
            InputStream in = new ByteArrayInputStream("<TOTO>Invalid</TOTO2>".getBytes());
            FeatureLoader.loadFeatures(in);
            fail();
        } catch (Exception ioex) {}
    }

    @Test
    public void testNullFile() {
        try {
            FeatureLoader.loadFeatures(null);
            fail();
        } catch (Exception ioex) {}
    }

    @Test
    public void testLoaderLoadInvalidXML() {
        try {
            InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-bad1.xml");
            FeatureLoader.loadFeatures(in);
            fail();
        } catch (Exception ioex) {}
    }

    @Test
    public void testLoaderInvalid2() {
        try {
            InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-bad2.xml");
            FeatureLoader.loadFeatures(in);
            fail();
        } catch (Exception ioex) {}
    }

    @Test
    public void testLoader4() {
        try {
            InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-bad3.xml");
            FeatureLoader.loadFeatures(in);
            fail();
        } catch (Exception ioex) {}
    }

    @Test
    public void testEmptyFile() {
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j-empty.xml");
        FeatureLoader.loadFeatures(in);
    }

    @Test
    public void testLoadConfigurationFile() throws IOException {
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j.xml");
        Map<String, Feature> maps = FeatureLoader.loadFeatures(in);
        Assert.assertEquals(5, maps.size());
        Assert.assertEquals(2, maps.get("forth").getAuthorizations().size());
    }

    @Test
    public void testExportCOnfiguration() throws IOException {
        // Import
        InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j.xml");
        Map<String, Feature> maps = FeatureLoader.loadFeatures(in);
        FeatureLoader.exportFeatures(maps);
    }

    @Test
    public void testExportFeatures() throws IOException {
        FeatureLoader.exportFeatures(new InMemoryFeatureStore("ff4j.xml").readAll());
        FeatureLoader.exportFeatures(new LinkedHashMap<String, Feature>());
        FeatureLoader.exportFeatures(null);
    }

}
