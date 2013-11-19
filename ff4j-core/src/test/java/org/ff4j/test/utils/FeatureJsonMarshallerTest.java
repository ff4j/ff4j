package org.ff4j.test.utils;

/*
 * #%L
 * ff4j-web
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import junit.framework.Assert;

import org.codehaus.jackson.JsonGenerationException;
import org.codehaus.jackson.map.JsonMappingException;
import org.codehaus.jackson.map.ObjectMapper;
import org.ff4j.core.Feature;
import org.ff4j.strategy.PonderationFlipStrategy;
import org.ff4j.utils.FeatureJsonMarshaller;
import org.junit.Test;

/**
 * Test class for JSON producer and consumer : {@link FeatureJsonMarshaller}
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureJsonMarshallerTest {

    /**
     * Jackson Mapper
     */
    protected ObjectMapper mapper = new ObjectMapper();

    /**
     * Check serialized string against json serializer.
     * 
     * @param json
     *            json value
     * @param feat
     *            feature
     **/
    private void assertMarshalling(Feature feat) throws JsonGenerationException, JsonMappingException, IOException {
        Assert.assertTrue(mapper.canSerialize(Feature.class));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        mapper.writeValue(baos, feat);
        String expected = new StringBuilder().append(baos).toString();

        String serialized = feat.toString();
        Assert.assertEquals(expected, serialized);

        Feature nFeature = FeatureJsonMarshaller.unMarshallFeature(serialized);
        Assert.assertEquals(expected, nFeature.toString());
    }

    @Test
    public void testMarshall1() throws IOException {
        assertMarshalling(new Feature("f1"));
    }

    @Test
    public void testMarshall2() throws IOException {
        assertMarshalling(new Feature("f2", true));
    }

    @Test
    public void testMarshall3() throws IOException {
        assertMarshalling(new Feature("f3", true, "desc"));
    }

    @Test
    public void testMarshall4() throws IOException {
        Set<String> newSet = new HashSet<String>();
        newSet.add("role1");
        newSet.add("role2");
        Feature f4 = new Feature("f4", true, "descript", "groupee", newSet);

        // Initializing PonderationFlipStrategy
        Map<String, String> initParam = new HashMap<String, String>();
        initParam.put("weight", "0.6");
        initParam.put("weight2", "0.6");
        PonderationFlipStrategy pf = new PonderationFlipStrategy(0.6);
        pf.init("f4", initParam);
        f4.setFlippingStrategy(pf);
        // <----
        assertMarshalling(f4);
    }

}
