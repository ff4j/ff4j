package org.ff4j.test.parser;

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
import java.util.Map;

import org.junit.Assert;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureXmlParser;
import org.junit.Test;

/**
 * Unit Testing
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureXmlParserTest {
    @Test
    public void testLoaderXMLFile() {
        InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-ok.xml");
        Map<String, Feature> features = new FeatureXmlParser().parseConfigurationFile(in);

        Assert.assertEquals(7, features.size());
        Assert.assertTrue(features.containsKey("f0"));
        Assert.assertNotNull(features.get("f0").getDescription());
        Assert.assertNotNull(features.get("f0").getPermissions());
        Assert.assertEquals(2, features.get("f0").getPermissions().size());
        Assert.assertNotNull(features.get("f0").getFlippingStrategy());
        Assert.assertEquals(1, features.get("f0").getFlippingStrategy().getInitParams().size());
        Assert.assertNotNull(features.get("f0").getGroup());
        Assert.assertEquals("group3", features.get("f0").getGroup());
        Assert.assertTrue(features.containsKey("f1"));
        Assert.assertTrue(features.containsKey("f2"));
        Assert.assertTrue(features.containsKey("f3"));
        Assert.assertTrue(features.containsKey("f4"));
        Assert.assertTrue(features.containsKey("f5"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testSaxException() {
        InputStream in = new ByteArrayInputStream("<TOTO>Invalid</TOTO2>".getBytes());
        new FeatureXmlParser().parseConfigurationFile(in);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testNullFile() {
        new FeatureXmlParser().parseConfigurationFile(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testLoaderRequiredUid() {
        InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-ko-uidrequired.xml");
        new FeatureXmlParser().parseConfigurationFile(in);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testLoaderRequiredEnable() {
        InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-ko-enablerequired.xml");
        new FeatureXmlParser().parseConfigurationFile(in);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testLoaderLoadInvalidStream() throws IOException {
        InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-ok.xml");
        in.close();
        new FeatureXmlParser().parseConfigurationFile(in);
    }

    @Test
    public void importThenExport() throws IOException {
        // Given
        FeatureXmlParser parser = new FeatureXmlParser();
        InputStream in = getClass().getClassLoader().getResourceAsStream("test-featureXmlParserTest-import-export.xml");
        Map<String, Feature> features = parser.parseConfigurationFile(in);
        Assert.assertNotNull(features);
        // When
        InputStream in2 = parser.exportFeatures(features);
        // Then
        // output is OK
        Map<String, Feature> features2 = parser.parseConfigurationFile(in2);
        Assert.assertNotNull(features2);
        Assert.assertEquals(features.size(), features2.size());
    }


}
